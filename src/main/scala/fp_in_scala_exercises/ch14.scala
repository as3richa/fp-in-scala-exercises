package fp_in_scala_exercises
import ch8.{SGen, Gen, forAll, run}
import ch12.Monad
import scala.reflect.ClassTag

object ch14 {
  def quicksort(xs: List[Int]): List[Int] = {
    val ary = xs.toArray

    def quicksort0(from: Int, to: Int): Unit =
      if (from < to) {
        val pivot = ary(from)
        ary(from) = ary(to)
        ary(to) = pivot

        val smaller = from.until(to).foldLeft(from) { (k, i) =>
          val x = ary(i)
          if (x < pivot) {
            ary(i) = ary(k)
            ary(k) = x
            k + 1
          } else {
            k
          }
        }

        ary(to) = ary(smaller)
        ary(smaller) = pivot

        quicksort0(from, smaller - 1)
        quicksort0(smaller + 1, to)
      }

    quicksort0(0, ary.length - 1)
    ary.toList
  }

  def testQuicksort: Unit =
    run(forAll(SGen.listOf(Gen.int)) { list =>
      quicksort(list) == list.sorted
    })

  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] =
      flatMap(a => ST(f(a)))

    def flatMap[B](f: A => ST[S, B]): ST[S, B] =
      new ST[S, B] {
        protected def run(s: S): (B, S) = {
          val (a, s2) = self.run(s)
          f(a).run(s2)
        }
      }
  }

  trait RunabbleST[A] {
    def apply[S]: ST[S, A]
  }

  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        protected def run(s: S): (A, S) = (memo, s)
      }
    }

    def monad[S]: Monad[({ type T[A] = ST[S, A] })#T] =
      new Monad[({ type T[A] = ST[S, A] })#T] {
        def unit[A](a: => A): ST[S, A] = ST(a)

        override def flatMap[A, B](a: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
          a.flatMap(f)
      }

    def run[A](a: RunabbleST[A]): A =
      a.apply.run(())._1
  }

  sealed trait STRef[S, A] {
    var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = ST { cell = a }
  }

  object STRef {
    def apply[S, A](a: => A): ST[S, STRef[S, A]] =
      ST(new STRef[S, A] {
        var cell = a
      })
  }

  val p1: RunabbleST[(Int, Int)] = new RunabbleST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]

    def size: ST[S, Int] =
      ST { value.size }

    def read(i: Int): ST[S, A] =
      ST { value(i) }

    def write(i: Int, a: A): ST[S, Unit] =
      ST { value(i) = a }

    def copy(i: Int, j: Int): ST[S, Unit] =
      ST { value(i) = value(j) }

    def fill(pairs: Map[Int, A]): ST[S, Unit] =
      pairs.foldLeft(ST[S, Unit](())) {
        case (s, i -> a) => s.flatMap(_ => write(i, a))
      }

    def freeze: ST[S, List[A]] =
      ST { value.toList }
  }

  object STArray {
    def apply[S, A](
        as: => List[A]
    )(implicit manifest: Manifest[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = as.toArray
      })

    def apply[S, A](sz: Int, a: A)(implicit
        manifest: Manifest[A]
    ): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = Array.fill(sz)(a)
      })
  }

  def quicksort2[A](
      as: List[A]
  )(implicit ord: Ordering[A], manifest: Manifest[A]): List[A] = {
    def quicksort0[S](ary: STArray[S, A], from: Int, to: Int): ST[S, Unit] =
      if (from < to) {
        for {
          pivot <- ary.read(from)
          _ <- ary.copy(from, to)
          _ <- ary.write(to, pivot)
          k <- partition(ary, pivot, from, to)
          _ <- ary.copy(to, k)
          _ <- ary.write(k, pivot)
          _ <- quicksort0(ary, from, k - 1)
          _ <- quicksort0(ary, k + 1, to)
        } yield ()
      } else {
        ST(())
      }

    def partition[S](
        ary: STArray[S, A],
        pivot: A,
        from: Int,
        to: Int
    ): ST[S, Int] =
      from.until(to).foldLeft(ST[S, Int](from)) { (sk, i) =>
        for {
          k <- sk
          a <- ary.read(i)
          k2 <-
            if (ord.lt(a, pivot)) {
              for {
                _ <- ary.copy(i, k)
                _ <- ary.write(k, a)
              } yield k + 1
            } else {
              ST[S, Int](k)
            }
        } yield k2
      }

    val sorted = new RunabbleST[List[A]] {
      def apply[S]: ST[S, List[A]] =
        for {
          ary <- STArray(as)
          length <- ary.size
          _ <- quicksort0(ary, 0, length - 1)
          sorted <- ary.freeze
        } yield sorted
    }

    ST.run(sorted)
  }

  def testQuicksort2: Unit =
    run(forAll(SGen.listOf(Gen.int)) { list =>
      quicksort2(list) == list.sorted
    })

  sealed trait STMap[S, K, V] {
    protected def hsh: collection.mutable.HashMap[K, V]

    def size: ST[S, Int] =
      ST { hsh.size }

    def read(x: K): ST[S, Option[V]] =
      ST { hsh.get(x) }

    def write(x: K, v: V): ST[S, Unit] =
      ST { hsh(x) = v }

    def copy(x: K, y: K): ST[S, Unit] =
      ST { hsh(x) = hsh(y) }

    def remove(x: K): ST[S, Option[V]] =
      ST { hsh.remove(x) }

    def keys: ST[S, Seq[K]] =
      ST { hsh.keys.toSeq }

    def freeze: ST[S, Map[K, V]] =
      ST { hsh.toMap }
  }

  object STMap {
    def apply[S, K, V]: ST[S, STMap[S, K, V]] =
      ST(new STMap[S, K, V] {
        val hsh = collection.mutable.HashMap()
      })

    def apply[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] =
      ST(new STMap[S, K, V] {
        lazy val hsh = (collection.mutable.HashMap.newBuilder ++= m).result()
      })
  }
}
