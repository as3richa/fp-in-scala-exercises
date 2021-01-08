package fp_in_scala_exercises
import java.util.concurrent.{Future, ExecutorService, TimeUnit}

object ch7 {
  def sum0(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  def sum00(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (left, right) = ints.splitAt(ints.size / 2)
      sum00(left) + sum00(right)
    }

  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](a: A) extends Future[A] {
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isCancelled(): Boolean = false
    def isDone(): Boolean = true
    def get(): A = a
    def get(timeout: Long, unit: TimeUnit): A = a
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C)
      extends Future[C] {
    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) && b.cancel(evenIfRunning)
    def isCancelled(): Boolean = a.isCancelled() && b.isCancelled()
    def isDone(): Boolean = a.isDone() && b.isDone()
    def get(): C = f(a.get, b.get)
    def get(timeout: Long, unit: TimeUnit): C = {
      val startedAt = System.nanoTime()
      val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, unit)
      val va = a.get(timeoutNs, TimeUnit.NANOSECONDS)
      val elapsedNs = System.nanoTime() - startedAt
      val vb = b.get(timeoutNs - elapsedNs, TimeUnit.NANOSECONDS)
      f(va, vb)
    }
  }

  object Par {
    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork { unit(a) }

    def run[A](ex: ExecutorService)(a: Par[A]): Future[A] = a(ex)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      ex => {
        Map2Future(Par.run(ex)(a), Par.run(ex)(b), f)
      }

    def fork[A](a: => Par[A]): Par[A] =
      ex => {
        ex.submit(() => a(ex).get)
      }

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def map[A, B](a: Par[A])(f: A => B): Par[B] =
      map2(a, unit(()))((a, _) => f(a))

    def sortPar(list: Par[List[Int]]): Par[List[Int]] =
      map(list)((li) => li.sorted)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
      val parBs = as.map(asyncF(f))
      sequence(parBs)
    }

    def sequence[A](parAs: List[Par[A]]): Par[List[A]] =
      parAs.foldRight(unit(List.empty[A])) {
        case (parA, parList) => map2(parA, parList)(_ :: _)
      }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val af = asyncF(f)
      val parOpts = sequence(
        as.map(a =>
          map(af(a))(cond =>
            if (cond) {
              Some(a)
            } else {
              None
            }
          )
        )
      )
      map(parOpts)(opts => opts.flatten)
    }

    def parReduceAssoc[A](as: IndexedSeq[A])(z: A)(f: (A, A) => A): Par[A] =
      if (as.size <= 1) {
        unit(as.headOption.getOrElse(z))
      } else {
        val (left, right) = as.splitAt(as.size / 2)
        map2(
          fork { parReduceAssoc(left)(z)(f) },
          fork { parReduceAssoc(right)(z)(f) }
        )((leftRes, rightRes) => {
          f(leftRes, rightRes)
        })
      }

    def parMapReduce[A, B, C](
        as: List[A]
    )(f: A => B)(z: C)(g: (B, C) => C): Par[C] =
      fork {
        map(parMap(as)(f))(bs => bs.foldRight(z)(g))
      }

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(
        f: (A, B, C) => D
    ): Par[D] =
      map2(map2(a, b)((_, _)), c) {
        case ((a, b), c) => f(a, b, c)
      }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(
        f: (A, B, C, D) => E
    ): Par[E] =
      map2(map3(a, b, c)((_, _, _)), d) {
        case ((a, b, c), d) => f(a, b, c, d)
      }

    def map5[A, B, C, D, E, F](
        a: Par[A],
        b: Par[B],
        c: Par[C],
        d: Par[D],
        e: Par[E]
    )(f: (A, B, C, D, E) => F): Par[F] =
      map2(map4(a, b, c, d)((_, _, _, _)), e) {
        case ((a, b, c, d), e) => f(a, b, c, d, e)
      }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit { ints.headOption.getOrElse(0) }
    } else {
      val (left, right) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork { sum(left) }, Par.fork { sum(right) })(_ + _)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    Par.parReduceAssoc(ints)(0)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    Par.parReduceAssoc(ints)(Int.MinValue)(Math.max(_, _))

  def words(paragraphs: List[String]): Par[Int] =
    Par.parMapReduce(paragraphs)(_.count(_ == ' '))(0)(_ + _)
}
