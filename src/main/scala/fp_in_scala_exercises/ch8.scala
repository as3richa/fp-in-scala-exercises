package fp_in_scala_exercises
import ch6.{RNG, SimpleRNG}
import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}

object ch8 {
  sealed trait Gen[+A] {
    def sample(rng: RNG): (A, RNG)

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      val g = this
      val bDomain = domain.flatMap {
        _.foldLeft(Option(List.empty[B])) { (bs, a) =>
          bs.flatMap(bs =>
            f(a).domain.flatMap(moreBs =>
              if (bs.size + moreBs.size <= Gen.maxFiniteDomainSize) {
                Some(bs ++ moreBs)
              } else {
                None
              }
            )
          )
        }
      }

      new Gen[B] {
        def sample(rng: ch6.RNG): (B, ch6.RNG) = {
          val (a, rng2) = g.sample(rng)
          f(a).sample(rng2)
        }

        def domain: Option[List[B]] = bDomain
      }
    }

    def map[B](f: A => B): Gen[B] = {
      val g = this
      val bDomain = domain.map(_.map(f))

      flatMap(a => Gen.unit(f(a)))
    }

    def domain: Option[List[A]]

    def listOfN(n: Gen[Int]): Gen[List[A]] =
      n.flatMap(n => Gen.listOfN(n, this))

    def unsized: SGen[A] = SGen(_ => this)
  }

  object Gen {
    val maxFiniteDomainSize = 10000

    def unit[A](a: A): Gen[A] =
      new Gen[A] {
        def sample(rng: ch6.RNG): (A, ch6.RNG) = (a, rng)

        def domain: Option[List[A]] = Some(List(a))
      }

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      new Gen[Int] {
        def sample(rng: RNG): (Int, RNG) =
          ch6.map(ch6.nonNegativeLessThan(stopExclusive - start))(_ + start)(
            rng
          )

        def domain: Option[List[Int]] =
          if (stopExclusive - start <= maxFiniteDomainSize) {
            Some(start.until(stopExclusive).toList)
          } else {
            None
          }
      }

    def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
      new Gen[List[A]] {
        def sample(rng: RNG): (List[A], RNG) =
          ch6.sequence(List.fill(n)(a.sample(_)))(rng)

        def domain: Option[List[List[A]]] = {
          a.domain.flatMap { as =>
            def lists(n: Int): List[List[A]] =
              if (n == 0) {
                List(Nil)
              } else {
                lists(n - 1).flatMap(list => as.map(_ :: list))
              }

            if (math.pow(as.size, n) <= maxFiniteDomainSize.toDouble) {
              Some(lists(n))
            } else {
              None
            }
          }
        }
      }

    val boolean: Gen[Boolean] =
      new Gen[Boolean] {
        def sample(rng: RNG): (Boolean, RNG) = ch6.map(ch6.int)(_ % 2 == 0)(rng)

        def domain: Option[List[Boolean]] = Some(List(false, true))
      }

    val double: Gen[Double] =
      new Gen[Double] {
        def sample(rng: ch6.RNG): (Double, ch6.RNG) = ch6.double2(rng)

        def domain: Option[List[Double]] = None
      }

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

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      double.flatMap { value =>
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

  case object Proved extends Result {
    def isFalsified: Boolean = false
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
          case Proved =>
            right.run(maxSize, cases, rng) match {
              case Proved       => Proved
              case Passed       => Passed
              case f: Falsified => f
            }
          case Passed =>
            right.run(maxSize, cases, rng) match {
              case Proved | Passed => Passed
              case f: Falsified    => f
            }
          case f: Falsified => f
        }
      }

    def ||(right: Prop): Prop =
      Prop { (maxSize, cases, rng) =>
        run(maxSize, cases, rng) match {
          case Proved => Proved
          case Passed => Passed
          case _: Falsified =>
            right.run(maxSize, cases, rng) match {
              case Proved       => Proved
              case Passed       => Passed
              case f: Falsified => f
            }
        }
      }

    def withTag(tag: String) =
      Prop {
        run(_, _, _) match {
          case Proved => Proved
          case Passed => Passed
          case Falsified(failedCase, successes) =>
            Falsified(s"[$tag] $failedCase", successes)
        }
      }
  }

  object Prop {
    def check(p: => Boolean): Prop = {
      lazy val ok = p
      Prop { (maxSize, cases, rng) =>
        if (ok) {
          Proved
        } else {
          Falsified(p.toString, 0)
        }
      }
    }
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    Prop { (maxSize, cases, rng) =>
      {
        val (as, exhaustive) =
          a.domain.filter(_.size <= cases).map((_, true)).getOrElse {
            val (as, _) = ch6.sequence(List.fill(cases)(a.sample(_)))(rng)
            (as, false)
          }

        @tailrec
        def forAll0(as: List[A], successes: Int): Result =
          as match {
            case a :: tail =>
              Try { f(a) } match {
                case Success(true) => forAll0(tail, successes + 1)
                case Success(false) =>
                  Falsified(a.toString, successes)
                case Failure(thrown: Exception) =>
                  Falsified(
                    buildErrorMessage(
                      a.toString,
                      thrown
                    ),
                    successes
                  )
                case Failure(thrown: Throwable) => throw thrown
              }
            case Nil =>
              if (exhaustive) { Proved }
              else {
                Passed
              }
          }
        forAll0(as, 0)
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

      val casesPerSize = (cases + maxSize) / (maxSize + 1)
      val max = maxSize.min(cases - 1)
      val props = List.tabulate(max + 1)(size => forAll(a(size))(f))

      props.reduceLeft(_ && _).run(maxSize, casesPerSize, rng) match {
        case Proved =>
          if (max == maxSize) {
            Proved
          } else {
            Passed
          }
        case res => res
      }
    }

  def run(
      prop: Prop,
      maxSize: Int = 100,
      cases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ) =
    prop.run(maxSize, cases, rng) match {
      case Proved => println(s"+ OK, proved property.")
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
      val g = SGen.listOf(Gen.choose(0, 5))
      run(
        forAll(g) { list =>
          val sorted = list.sorted
          list.maxOption == sorted.maxOption && list.minOption == sorted.minOption && list.length == sorted.length
        },
        5,
        1000000
      )

      run(
        forAll(g) { list =>
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
        },
        5
      )
    }

    object par {
      import ch7.nonblocking.Par
      import java.util.concurrent.ExecutorService
      import java.util.concurrent.Executors

      def testMap = {
        val ex = Executors.newCachedThreadPool

        run(
          Prop.check {
            Par.run(ex)(Par.map(Par.unit(1))(_ + 1)) == Par.run(ex)(Par.unit(2))
          }
        )
      }
    }
  }
}
