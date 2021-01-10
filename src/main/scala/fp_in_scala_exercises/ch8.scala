package fp_in_scala_exercises
import ch6.{RNG, SimpleRNG, sequence, nonNegativeLessThan, double}
import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}

object ch8 {
  case class State[S, +A](run: S => (A, S)) {
    def apply(s: S): (A, S) = run(s)
  }

  case class Gen[+A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(State(rng => {
        val (a, rng2) = sample(rng)
        val (b, rng3) = f(a).sample(rng2)
        (b, rng3)
      }))

    def map[B](f: A => B): Gen[B] =
      flatMap(a => Gen.unit(f(a)))

    def listOfN(n: Gen[Int]): Gen[List[A]] =
      n.flatMap(n => Gen.listOfN(n, this))

    def unsized: SGen[A] = SGen(_ => this)
  }

  object Gen {
    def unit[A](a: A): Gen[A] = Gen(State(rng => (a, rng)))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(rng => nonNegativeLessThan(stopExclusive - start)(rng)))

    def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
      Gen(State(rng => {
        @tailrec
        def listOfN0(n: Int, list: List[A], rng: RNG): (List[A], RNG) =
          if (n == 0) {
            (list, rng)
          } else {
            val (value, rng2) = a.sample(rng)
            listOfN0(n - 1, value :: list, rng2)
          }

        listOfN0(n, Nil, rng)
      }))

    def listOfN2[A](n: Int, a: Gen[A]): Gen[List[A]] =
      Gen(State(sequence(List.fill(n)(rng => a.sample(rng)))))

    def boolean: Gen[Boolean] =
      Gen(State(rng => {
        val (n, rng2) = rng.nextInt
        (n % 2 == 0, rng2)
      }))

    def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
      a.flatMap(a => b.flatMap(b => unit(f(a, b))))

    def pair[A](a: Gen[A]) =
      map2(a, a)((_, _))

    def wrapOption[A](a: Gen[A]): Gen[Option[A]] = a.map(Some(_))

    def unwrapOption[A](a: Gen[Option[A]]): Gen[A] = a.map(_.get)

    def union[A](a1: Gen[A], a2: Gen[A]): Gen[A] =
      boolean.flatMap {
        if (_) a1 else a2
      }

    val doubl = Gen(State(rng => double(rng)))

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      doubl.flatMap { value =>
        val (a1, p) = g1
        val (a2, q) = g2
        if ((value * (p + q)) < p) {
          a1
        } else {
          a2
        }
      }
  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen { size =>
        forSize(size).flatMap(f(_).forSize(size))
      }

    def map[B](f: A => B): SGen[B] =
      SGen { size =>
        forSize(size).map(f)
      }
  }

  object SGen {
    def unit[A](a: A): SGen[A] = SGen(_ => Gen.unit(a))

    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen { size => Gen.listOfN(size, g) }

    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen { size => Gen.listOfN(size.max(1), g) }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failedCase: String, successes: Int) extends Result {
    def isFalsified: Boolean = true
  }

  case class Prop(run: (Int, Int, RNG) => Result) {
    def &&(right: Prop): Prop =
      Prop { (maxSize, cases, rng) =>
        run(maxSize, cases, rng) match {
          case Passed =>
            right.run(maxSize, cases, rng) match {
              case Passed  => Passed
              case failure => failure
            }
          case failure => failure
        }
      }

    def ||(right: Prop): Prop =
      Prop { (maxSize, cases, rng) =>
        run(maxSize, cases, rng) match {
          case Passed => Passed
          case failure =>
            right.run(maxSize, cases, rng) match {
              case Passed  => Passed
              case failure => failure
            }
        }
      }

    def withTag(tag: String) =
      Prop {
        run(_, _, _) match {
          case Passed => Passed
          case Falsified(failedCase, successes) =>
            Falsified(s"[$tag] $failedCase", successes)
        }
      }
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    Prop { (maxSize, cases, rng) =>
      {
        @tailrec
        def forAll0(successes: Int, rng: RNG): Result = {
          if (successes >= cases) {
            Passed
          } else {
            val (value, rng2) = a.sample(rng)
            Try { f(value) } match {
              case Success(true) => forAll0(successes + 1, rng2)
              case Success(false) =>
                Falsified(value.toString, successes)
              case Failure(thrown: Exception) =>
                Falsified(
                  buildErrorMessage(
                    value.toString,
                    thrown
                  ),
                  successes
                )
              case Failure(thrown: Throwable) => throw thrown
            }
          }
        }
        forAll0(0, rng)
      }
    }

  def forAll[A](a: SGen[A])(f: A => Boolean): Prop =
    forAll(a.forSize(_))(f)

  def forAll[A](a: Int => Gen[A])(
      f: A => Boolean
  ): Prop =
    Prop { (maxSize, cases, rng) =>
      if (cases <= 0) {
        throw new IllegalArgumentException("cases <= 0")
      }

      val casesPerSize = (cases + maxSize - 1) / maxSize
      val props =
        List.tabulate(maxSize.min(cases) + 1)(size => forAll(a(size))(f))

      val prop = props
        .map(prop =>
          Prop { (maxSize, casesPerSize, rng) =>
            prop.run(maxSize, casesPerSize, rng)
          }
        )
        .reduceLeft(_ && _)

      prop.run(maxSize, casesPerSize, rng)
    }

  def run(
      prop: Prop,
      maxSize: Int = 100,
      cases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ) =
    prop.run(maxSize, cases, rng) match {
      case Passed => println(s"+ OK, passed $cases tests")
      case Falsified(failedCase, successes) =>
        println(s"! Falsified after $successes passed tests:\n  $failedCase")
    }

  def buildErrorMessage(cse: String, thrown: Exception) =
    s"test case: $cse\n" +
      s"generated an exception: ${thrown.getMessage}\n" +
      s"stack trace:\n ${thrown.getStackTrace.mkString("\n")}"

  object tests {
    def testMax = {
      val maxGe = forAll(SGen.listOf1(Gen.choose(-100, 100))) { list =>
        val max = list.max
        !list.exists(_ > max)
      }
      run(maxGe)
    }

    def testSort = {
      val g = SGen.listOf(Gen.choose(-1000, 1000))
      run(forAll(g) { list =>
        val sorted = list.sorted
        list.maxOption == sorted.maxOption && list.minOption == sorted.minOption && list.length == sorted.length
      })

      run(forAll(g) { list =>
        @tailrec
        def isSorted(list: List[Int], prev: Option[Int]): Boolean =
          list match {
            case head :: tail =>
              (prev match {
                case Some(prev) => prev <= head
                case None       => true
              }) && isSorted(tail, Some(head))
            case Nil => true
          }

        isSorted(list.sorted, None)
      })
    }
  }
}
