package fp_in_scala_exercises
import ch6.{RNG, SimpleRNG}
import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}
import scala.collection.immutable.Nil
import scala.util.hashing.MurmurHash3

object ch8 {
  sealed trait Gen[+A] {
    def sample(rng: RNG): (A, RNG)

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      val g = this
      lazy val bDomain = domain.flatMap {
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

    def map[B](f: A => B): Gen[B] =
      flatMap(a => Gen.unit(f(a)))

    def domain: Option[List[A]]

    def listOfN(n: Gen[Int]): Gen[List[A]] =
      n.flatMap(n => Gen.listOfN(n, this))

    def unsized: SGen[A] = SGen(_ => this)

    def **[B](b: Gen[B]): Gen[(A, B)] =
      Gen.map2(this, b)((_, _))
  }

  object Gen {
    val maxFiniteDomainSize = 10000

    def unit[A](a: => A): Gen[A] =
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

    val int: Gen[Int] = new Gen[Int] {
      def sample(rng: RNG): (Int, RNG) = rng.nextInt

      def domain: Option[List[Int]] = None
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

    val char: Gen[Char] = choose(32, 127).map(_.toChar)

    def stringN(n: Int): Gen[String] = listOfN(n, char).map(_.mkString)

    val string: Gen[String] = choose(0, 200).flatMap(stringN(_))

    def map2[A, B, C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
      a.flatMap(a => b.flatMap(b => unit(f(a, b))))

    def pair[A](a: Gen[A]) =
      map2(a, a)((_, _))

    def wrapOption[A](a: Gen[A]): Gen[Option[A]] = a.map(Some(_))

    def unwrapOption[A](a: Gen[Option[A]]): Gen[A] = a.map(_.get)

    def option[A](a: Gen[A]): Gen[Option[A]] =
      boolean.flatMap {
        if (_) {
          a.map(Some(_))
        } else {
          unit(None)
        }
      }

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

    def weighted[A](gs: (Gen[A], Double)*): Gen[A] = {
      @tailrec
      def pick(p: Double, gs: List[(Gen[A], Double)]): Gen[A] =
        gs match {
          case (a, _) :: Nil => a
          case (a, q) :: tail =>
            if (p < q) {
              a
            } else {
              pick(p - q, tail)
            }
          case Nil => throw new Exception("gs empty")
        }

      double.flatMap { x =>
        val p = x * gs.map(_._2).sum
        pick(p, gs.toList)
      }
    }

    def lazily[A](a: => Gen[A]): Gen[A] = {
      lazy val g = a;
      new Gen[A] {
        def sample(rng: ch6.RNG): (A, ch6.RNG) = g.sample(rng)
        def domain: Option[List[A]] = g.domain
      }
    }

    def recursive[A](a: Gen[A] => Gen[A]): Gen[A] = {
      var g: Gen[A] = null;
      g = a(lazily(g))
      g
    }

    val stringToIntFunction: Gen[String => Int] =
      function1[Int, String, Int](
        int,
        (seed, str) => MurmurHash3.orderedHash(str, seed)
      )

    def function1[S, A, B](seed: Gen[S], hash: (S, A) => B): Gen[A => B] =
      seed.map(seed => hash(seed, _))
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

    def **[B](b: SGen[B]): SGen[(A, B)] =
      SGen.map2(this, b)((_, _))
  }

  object SGen {
    def unit[A](a: A): SGen[A] = SGen(_ => Gen.unit(a))

    def map2[A, B, C](a: SGen[A], b: SGen[B])(f: (A, B) => C) =
      SGen { size =>
        Gen.map2(a.forSize(size), b.forSize(size))(f)
      }

    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen { size => Gen.listOfN(size, g) }

    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen { size => Gen.listOfN(size.max(1), g) }

    def growable[A](base: Gen[A], grow: A => Gen[A]): SGen[A] = {
      SGen(size =>
        base.flatMap(b =>
          0.until(size).foldLeft(Gen.unit(b)) {
            case (a, _) => a.flatMap(grow)
          }
        )
      )
    }

    def string: SGen[String] = listOf(Gen.char).map(_.mkString)
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

    val intsFn: Gen[(Int, Int) => Int] =
      Gen.weighted(
        Seq[(Int, Int) => Int](
          _ + _,
          _ - _,
          _ * _,
          (x, y) => if (y == 0) Int.MaxValue else x / y,
          (x, y) => if (y == 0) Int.MaxValue else x % y,
          _ & _,
          _ | _,
          _ ^ _,
          (x, y) => x,
          (x, y) => y
        ).map(Gen.unit(_) -> 1.0): _*
      )

    object par {
      import ch7.nonblocking.Par
      import java.util.concurrent.ExecutorService
      import java.util.concurrent.Executors

      def equal[A](a1: Par[A], a2: Par[A]): Par[Boolean] =
        Par.map2(a1, a2)(_ == _)

      implicit def parToEqualable[A](a: Par[A]): Equalable[A] = Equalable(a)

      case class Equalable[A](a: Par[A]) {
        def ===(a2: Par[A]): Par[Boolean] = equal(a, a2)
      }

      val ex = Gen.weighted(
        Gen.choose(1, 5).map(Executors.newFixedThreadPool(_)) -> 0.75,
        Gen.unit { Executors.newCachedThreadPool } -> 0.25
      )

      def forAllPar[A](a: Gen[A])(p: A => Par[Boolean]): Prop = {
        forAll(a ** ex) {
          case (a, ex) =>
            Par.run(ex)(p(a))
        }
      }

      def forAllPar[A](a: SGen[A])(p: A => Par[Boolean]): Prop = {
        forAll(a ** ex.unsized) {
          case (a, ex) =>
            Par.run(ex)(p(a))
        }
      }

      def checkPar(p: => Par[Boolean]): Prop = {
        forAll(ex) { ex =>
          Par.run(ex)(p)
        }
      }

      def testMap = {
        run(
          checkPar {
            Par.map(Par.unit(1))(_ + 1) === Par.unit(2)
          }
        )

        run(
          forAllPar(Gen.double.map(Par.unit(_)))(x => Par.map(x)(a => a) === x)
        )
      }

      val parInt: Gen[Par[Int]] = Gen.recursive { parInt: Gen[Par[Int]] =>
        Gen.weighted(
          Gen.choose(-100000000, 100000000).map(Par.unit(_)) -> 1.0,
          Gen.map2(Gen.pair(Gen.lazily(parInt)), intsFn) {
            case ((x, y), f) => Par.map2(x, y)(f)
          } -> 1.0,
          Gen.lazily(parInt).map(Par.fork(_)) -> 1.0
        )
      }

      def testFork = {
        run(forAllPar(parInt)(x => x === Par.fork { x }))
      }
    }

    val intPred: Gen[Int => Boolean] = {
      val baseIntPreds: Seq[Int => Boolean] = 0.until(32).map {
        bit => (x: Int) =>
          ((x >> bit) & 1) != 0
      } ++ Seq(_ => false, _ => true)

      val binaryOps: Seq[(Boolean, Boolean) => Boolean] =
        Seq(
          _ && _,
          _ || _,
          _ ^ _
        )

      Gen.recursive { (intPred: Gen[Int => Boolean]) =>
        Gen.weighted(
          (baseIntPreds.map(Gen.unit(_) -> 1.0) ++
            binaryOps.map(f =>
              Gen.pair(intPred).map {
                case (g, h) => (x: Int) => f(g(x), h(x))
              } -> 10.0
            ) :+ intPred.map(f => (x: Int) => !f(x)) -> 10.0): _*
        )
      }
    }

    def testTakeWhile = {
      run(
        forAll(
          SGen.listOf(Gen.choose(-1000000000, 1000000000)) ** intPred.unsized
        ) {
          case (list, pred) =>
            val taken = list.takeWhile(pred)
            val dropped = list.dropWhile(pred)
            taken ++ dropped == list &&
            taken.forall(pred(_)) &&
            dropped.headOption.map(!pred(_)).getOrElse(true)
        }
      )
    }

    def testList = {
      val g = SGen.listOf(Gen.int)
      val props = Seq(
        forAll(g ** g) {
          case (x, y) =>
            val z = x.zip(y)
            z.map(_._1) == x.take(y.size) &&
            z.map(_._2) == y.take(x.size)
        },
        forAll(
          g.flatMap(x => (Gen.unit(x) ** Gen.choose(0, x.size + 1)).unsized)
        ) {
          case (x, n) =>
            x.take(n) == x.reverse.drop(x.size - n).reverse
        },
        forAll(g ** intPred.unsized) {
          case (x, f) =>
            @tailrec
            def mtch(x: List[Int], y: List[Int]): Boolean =
              (x, y) match {
                case (a :: x2, b :: y2) =>
                  if (a == b) {
                    mtch(x2, y2)
                  } else {
                    mtch(x2, y)
                  }
                case (Nil, _ :: _) => false
                case (_, Nil)      => true
              }
            mtch(x, x.filter(f))
        }
      )

      props.foreach(run(_))
    }

    import ch3.{Tree, Leaf, Branch}

    val tree: SGen[Tree[Int]] = {
      val base: Gen[Tree[Int]] = Gen.int.map(Leaf(_))

      def grow(tree: Tree[Int]): Gen[Tree[Int]] =
        tree match {
          case Leaf(value) => Gen.int.map(x => Branch(Leaf(x), Leaf(value)))
          case Branch(left, right) =>
            Gen.boolean.flatMap { cond =>
              if (cond) {
                grow(left).map(Branch(_, right))
              } else {
                grow(right).map(Branch(left, _))
              }
            }
        }

      SGen.growable(base, grow)
    }

    def flattenTree[A](tree: Tree[A]): List[A] =
      tree match {
        case Leaf(a)             => List(a)
        case Branch(left, right) => flattenTree(left) ++ flattenTree(right)
      }

    def testTree = {
      run(forAll(tree)((tree: Tree[Int]) => {
        Tree.fold(tree)(x => x)(_ + _) == flattenTree(tree).sum
      }))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(List.empty[A])) {
        case (aOpt, listOpt) =>
          aOpt.flatMap(a => listOpt.map(list => a :: list))
      }

    def testSequence = {
      run(forAll(SGen.listOf(Gen.option(Gen.int))) { list =>
        sequence(list) == (if (list.forall(_.isDefined)) {
                             Some(list.map(_.get))
                           } else {
                             None
                           })
      })

      run(forAll(SGen.listOf(Gen.wrapOption(Gen.int))) { list =>
        sequence(list) == Some(list.map(_.get))
      })
    }
  }
}
