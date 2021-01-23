package fp_in_scala_exercises
import java.util.concurrent.{
  Callable,
  CountDownLatch,
  Future,
  ExecutorService,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicReference

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

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      fork {
        val parBs = as.map(asyncF(f))
        sequence(parBs)
      }

    def sequence[A](parAs: List[Par[A]]): Par[List[A]] =
      parAs match {
        case parA :: tail => map2(parA, fork { sequence(tail) })(_ :: _)
        case Nil          => unit(Nil)
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

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      run(e)(p).get == run(e)(p2).get

    def delay[A](a: => Par[A]): Par[A] = ex => a(ex)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      ex => {
        run(ex)(choices(run(ex)(n).get))
      }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      ex => {
        run(ex)(choices(run(ex)(key).get))
      }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      ex => {
        run(ex)(f(run(ex)(a).get))
      }

    def choiceMap2[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      flatMap(key)(choices)

    def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(n)(choices(_))

    def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond) {
        if (_) {
          t
        } else {
          f
        }
      }

    def join[A](a: Par[Par[A]]): Par[A] =
      ex => {
        run(ex)(run(ex)(a).get)
      }

    def flatMap2[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      join(map(a)(f))

    def join2[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(a => a)

    def map2Doppel[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      flatMap(a)(a => {
        flatMap(b)(b => {
          unit(f(a, b))
        })
      })
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

  object nonblocking {
    sealed trait Future[+A] {
      private[nonblocking] def apply(f: Either[Throwable, A] => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    case class ParOps[A](par: Par[A]) {
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(par)(f(_))
      def map[B](f: A => B): Par[B] = Par.map(par)(f(_))
    }

    implicit def toParOps[A](par: Par[A]): ParOps[A] = ParOps(par)

    object Par {

      def run[A](ex: ExecutorService)(a: Par[A]): A = {
        val ref = new AtomicReference[Either[Throwable, A]]
        val latch = new CountDownLatch(1)
        a(ex) { res =>
          ref.set(res)
          latch.countDown()
        }
        latch.await
        ref.get match {
          case Right(a)     => a
          case Left(thrown) => throw thrown
        }
      }

      def async[A](f: (Either[Throwable, A] => Unit) => Unit): Par[A] =
        ex =>
          new Future[A] {
            private[nonblocking] def apply(
                g: Either[Throwable, A] => Unit
            ): Unit = f(g(_))
          }

      def unit[A](a: A): Par[A] =
        _ =>
          new Future[A] {
            private[nonblocking] def apply(
                f: Either[Throwable, A] => Unit
            ): Unit = f(Right(a))
          }

      def fork[A](a: => Par[A]): Par[A] =
        ex =>
          new Future[A] {
            private[nonblocking] def apply(
                f: Either[Throwable, A] => Unit
            ): Unit =
              eval(ex)(a(ex)(f))(thrown => f(Left(thrown)))
          }

      def eval[A](ex: ExecutorService)(f: => Unit)(g: Throwable => Unit): Unit =
        ex.submit(new Callable[Unit] {
          def call: Unit =
            try {
              f
            } catch {
              case thrown: Throwable => g(thrown)
            }
        })

      def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] =
        ex => {
          new Future[C] {
            private[nonblocking] def apply(
                g: Either[Throwable, C] => Unit
            ): Unit = {
              var aOpt = Option.empty[A]
              var bOpt = Option.empty[B]

              val actor = Actor[Either[A, B]](ex) { either =>
                either match {
                  case Left(a) =>
                    bOpt match {
                      case Some(b) =>
                        eval(ex)(g(Right(f(a, b))))((thrown => g(Left(thrown))))
                      case None => aOpt = Some(a)
                    }
                  case Right(b) =>
                    aOpt match {
                      case Some(a) =>
                        eval(ex)(g(Right(f(a, b))))((thrown => g(Left(thrown))))
                      case None => bOpt = Some(b)
                    }
                }
              }

              a(ex) {
                case Right(a)     => actor ! Left(a)
                case Left(thrown) => g(Left(thrown))
              }

              b(ex) {
                case Right(b)     => actor ! Right(b)
                case Left(thrown) => g(Left(thrown))
              }
            }
          }
        }

      def lazyUnit[A](a: => A): Par[A] =
        fork { unit(a) }

      def asyncF[A, B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

      def map[A, B](a: Par[A])(f: A => B): Par[B] =
        map2(a, unit(()))((a, _) => f(a))

      def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
        fork {
          val parBs = as.map(asyncF(f))
          sequence(parBs)
        }

      def sequence[A](parAs: List[Par[A]]): Par[List[A]] =
        parAs match {
          case parA :: tail => map2(parA, fork { sequence(tail) })(_ :: _)
          case Nil          => unit(Nil)
        }

      def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
        ex =>
          new Future[B] {
            private[nonblocking] def apply(
                g: Either[Throwable, B] => Unit
            ): Unit =
              a(ex) {
                case Right(a)     => (f(a))(ex)(g)
                case Left(thrown) => g(Left(thrown))
              }
          }

      def join[A](a: Par[Par[A]]): Par[A] =
        ex =>
          new Future[A] {
            private[nonblocking] def apply(
                f: Either[Throwable, A] => Unit
            ): Unit = {
              a(ex) {
                case Right(a)     => a(ex)(f)
                case Left(thrown) => f(Left(thrown))
              }
            }
          }

      def join2[A](a: Par[Par[A]]): Par[A] =
        flatMap(a)(a => a)
    }
  }
}
