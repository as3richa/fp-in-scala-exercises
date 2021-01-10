package fp_in_scala_exercises
import ch6.{RNG, sequence, nonNegativeLessThan, double}
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object ch8 {
  case class State[S, A](run: S => (A, S)) {
    def apply(s: S): (A, S) = run(s)
  }

  case class Gen[A](sample: State[RNG, A]) {
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

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failedCase: String, successes: Int) extends Result {
    def isFalsified: Boolean = true
  }

  case class Prop(run: (Int, RNG) => Result) {
    def &&(right: Prop): Prop =
      Prop { (cases, rng) =>
        this.run(cases, rng) match {
          case Passed =>
            right.run(cases, rng) match {
              case Passed  => Passed
              case failure => failure
            }
          case failure => failure
        }
      }

    def ||(right: Prop): Prop =
      Prop { (cases, rng) =>
        this.run(cases, rng) match {
          case Passed => Passed
          case failure =>
            right.run(cases, rng) match {
              case Passed  => Passed
              case failure => failure
            }
        }
      }
  }

  def forAll[A](a: Gen[A], tag: Option[String] = None)(f: A => Boolean): Prop =
    Prop { (cases, rng) =>
      {
        val prettyTag = tag.map(tag => s"[$tag] ").getOrElse("")

        @tailrec
        def forAll0(successes: Int, rng: RNG): Result = {
          if (successes >= cases) {
            Passed
          } else {
            val (value, rng2) = a.sample(rng)
            Try { f(value) } match {
              case Success(true) => forAll0(successes + 1, rng2)
              case Success(false) =>
                Falsified(prettyTag + value.toString, successes)
              case Failure(thrown) =>
                Falsified(
                  buildErrorMessage(
                    prettyTag + value.toString,
                    thrown.asInstanceOf[Exception]
                  ),
                  successes
                )
            }
          }
        }
        forAll0(0, rng)
      }
    }

  def buildErrorMessage(cse: String, thrown: Exception) =
    s"test case: $cse\n" +
      s"generated an exception: ${thrown.getMessage}\n" +
      s"stack trace:\n ${thrown.getStackTrace.mkString("\n")}"
}
