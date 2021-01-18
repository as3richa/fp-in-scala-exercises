package fp_in_scala_exercises
import ch5.Stream
import ch6.State
import ch7.nonblocking.Par
import ch8.{SGen, Gen, Prop, forAll, run}
import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

object ch11 {
  trait Functor[F[_]] {
    def map[A, B](as: F[A])(f: A => B): F[B]

    def distribute[A, B](xs: F[(A, B)]): (F[A], F[B]) =
      (map(xs)(_._1), map(xs)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(as)  => map(as)(Left(_))
        case Right(bs) => map(bs)(Right(_))
      }

    object FunctorLaws {
      def mapIdentity[A](gen: Gen[F[A]]): Prop =
        forAll(gen)(as => map(as)(x => x) == as)

      def mapAssociativity[A, B, C](
          gen: Gen[F[A]]
      )(f: B => C)(g: A => B): Prop =
        forAll(gen)(as => map(map(as)(g))(f) == map(as)(f.compose(g)))
    }
  }

  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](a: List[A])(f: A => B): List[B] =
      a.map(f)
  }

  val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f)
  }

  def testFunctorLaws: Unit = {
    val gen = Gen.int.listOfN(Gen.choose(0, 100))
    Seq(
      listFunctor.FunctorLaws.mapIdentity(gen),
      listFunctor.FunctorLaws.mapAssociativity(gen)((x: Int) => x + 13)(
        (x: Int) => x * 37
      )
    ).foreach(run(_))
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]
    def unit[A](a: => A): F[A]

    def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

    def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
      flatMap(a) { a =>
        map(b)(f(a, _))
      }

    def sequence[A](as: List[F[A]]): F[List[A]] =
      as.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, tail) => map2(f(a), tail)(_ :: _))

    def replicateM[A](n: Int, a: F[A]): F[List[A]] = {
      @tailrec
      def replicateM0(n: Int, list: F[List[A]]): F[List[A]] =
        if (n <= 0) {
          list
        } else {
          replicateM0(n - 1, map2(a, list)(_ :: _))
        }
      replicateM0(n, unit(Nil))
    }

    def product[A, B](a: F[A], b: F[B]): F[(A, B)] = map2(a, b)((_, _))

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
      as.foldRight(unit(List.empty[A])) { (a, tails) =>
        map2(f(a), tails) { (keep, tail) =>
          if (keep) {
            a :: tail
          } else {
            tail
          }
        }
      }

    def filterM2[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
      as match {
        case a :: tail =>
          flatMap(f(a)) { keep =>
            if (keep) {
              map(filterM2(tail)(f))(a :: _)
            } else {
              filterM2(tail)(f)
            }
          }
        case Nil => unit(List.empty[A])
      }

    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] =
      (a: A) => flatMap(f(a))(g(_))

    def flatMap2[A, B](a: F[A])(f: A => F[B]): F[B] =
      compose[Unit, A, B](_ => a)(f(_))(())

    def join[A](a: F[F[A]]): F[A] =
      flatMap(a)(a => a)

    def flatMap3[A, B](a: F[A])(f: A => F[B]): F[B] =
      join(map(a)(f))

    def compose2[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] =
      a => {
        join(map(f(a))(g(_)))
      }

    trait MonadEq {
      def apply[A](x: F[A], y: F[A]): Boolean
    }

    val defaultEq = new MonadEq {
      def apply[A](x: F[A], y: F[A]): Boolean = x == y
    }

    case class MonadLaws(eq: MonadEq = defaultEq) {
      def flatMapAssociativity[A, B, C](
          a: Gen[F[A]]
      )(f: A => F[B])(
          g: B => F[C]
      ): Prop =
        forAll(a) { a =>
          eq(
            flatMap(flatMap(a)(f(_)))(g(_)),
            flatMap(a)(a => flatMap(f(a))(g(_)))
          )
        }

      def flatMapAssociativity2[A, B, C, D](
          a: Gen[A]
      )(f: A => F[B])(g: B => F[C])(
          h: C => F[D]
      ): Prop =
        forAll(a) { a =>
          eq(
            compose(compose(f)(g))(h)(a),
            compose(f)(compose(g)(h))(a)
          )
        }

      def flatMapAssociativity3[A, B, C](
          a: Gen[F[F[F[A]]]]
      ): Prop =
        forAll(a) { a =>
          eq(
            join(join(a)),
            join(map(a)(join(_)))
          )
        }

      def flatMapCompose[A, B](a: Gen[F[A]])(f: A => F[B]): Prop =
        forAll(a) { a => eq(flatMap(a)(f(_)), flatMap2(a)(f(_))) }

      def unitIdentity[A, B](a: Gen[A])(
          f: A => F[B]
      ): Prop = {
        forAll(a) { a =>
          eq(compose(f)(unit(_))(a), f(a)) &&
          eq(compose(unit[A](_))(f)(a), f(a))
        }
      }

      def unitIdentity2[A, B](a: Gen[A])(f: A => F[B]): Prop =
        forAll(a) { a =>
          eq(flatMap(unit(a))(f(_)), f(a)) &&
          eq(flatMap(f(a))(unit(_)), f(a))
        }

      def unitIdentity3[A, B](a: Gen[F[A]])(f: A => F[B]): Prop =
        forAll(a) { a =>
          eq(join(map(a)(unit[A](_))), a) &&
          eq(join(unit(a)), a)
        }
    }
  }

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def flatMap[A, B](a: Gen[A])(f: A => Gen[B]): Gen[B] = a.flatMap(f)
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(a)(f)
    def unit[A](a: => A): ch7.nonblocking.Par[A] = Par.unit(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] =
      a.flatMap(f(_))
    def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] =
      a.flatMap(f(_))
    def unit[A](a: => A): Stream[A] = Stream.constant(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f(_))
    def unit[A](a: => A): List[A] = List(a)
  }

  def stateMonad[S]: Monad[({ type T[A] = State[S, A] })#T] =
    new Monad[({ type T[A] = State[S, A] })#T] {
      def flatMap[A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] =
        a.flatMap(f(_))

      def unit[A](a: => A): State[S, A] = State.unit(a)
    }

  case class StateMonad[S]() {
    type SState[A] = State[S, A]

    def monad[A]: Monad[SState] =
      new Monad[SState] {
        def flatMap[A, B](a: SState[A])(f: A => SState[B]): SState[B] =
          a.flatMap(f(_))

        def unit[A](a: => A): SState[A] = State.unit(a)
      }
  }

  case class Order(item: Item, quantity: Int)
  case class Item(name: String, price: Double)

  object Item {
    val gen: Gen[Item] =
      Gen.map2(Gen.stringN(3), Gen.double.map(_ * 10))(Item(_, _))
  }

  object Order {
    val gen: Gen[Order] = for {
      item <- Item.gen
      quantity <- Gen.choose(1, 5)
    } yield Order(item, quantity)
  }

  def testFilterM: Unit = {
    run(forAll(Gen.choose(0, 5).listOfN(Gen.choose(0, 5))) { xs =>
      val f = (x: Int) => {
        List.tabulate(x)(_ % 2 == 0)
      }
      listMonad.filterM(xs)(f) == listMonad.filterM2(xs)(f)
    })
  }

  def testMonadLaws: Unit = {
    val laws = listMonad.MonadLaws()

    val int = Gen.choose(0, 100)
    val list = Gen.int.listOfN(Gen.choose(0, 100))
    val list3 = list.listOfN(Gen.choose(0, 5)).listOfN(Gen.choose(0, 5))

    def f(x: Int): List[Int] =
      List.tabulate(x % 10)(x + _)

    def g(x: Int): List[Int] =
      List.tabulate(x % 5)(y => (1337 + y) * x)

    def h(x: Int): List[Int] =
      List.tabulate(x % 10)(_ => x)

    Seq(
      laws.flatMapAssociativity(list)(f)(g),
      laws.flatMapAssociativity2(int)(f)(g)(h),
      laws.flatMapAssociativity3(list3),
      laws.flatMapCompose(list)(g),
      laws.unitIdentity(int)(f),
      laws.unitIdentity2(int)(f),
      laws.unitIdentity3(list)(f)
    ).foreach(run(_))
  }

  case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  object Id {
    def unit[A](value: A): Id[A] = Id(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = a.flatMap(f(_))
    def unit[A](a: => A): Id[A] = Id.unit(a)
  }

  def testStateMonad: Unit = {
    val s = State { (count: Int) =>
      (count, count + 1)
    }

    val gs = Gen.choose(0, 100).map(n => (n, stateMonad.replicateM(n, s)))

    val rep = forAll(gs ** Gen.int) {
      case ((n, s), x) =>
        s.run(x) == (0.until(n).map(_ + x).toList, n + x)
    }

    val seq = forAll(gs.listOfN(Gen.choose(0, 100))) { g =>
      val (ns, ss) = g.unzip

      val prefixSums = ns
        .foldLeft(List(0)) {
          case (sums, n) => (sums.head + n) :: sums
        }
        .reverse

      val res = prefixSums.zip(prefixSums.drop(1)).map {
        case (x, y) => x.until(y).toList
      }

      stateMonad.sequence(ss).run(0) == (res, ns.sum)
    }

    val map2 = forAll(gs ** gs ** Gen.int) {
      case (((n1, s1), (n2, s2)), x) =>
        stateMonad.map2(s1, s2)((_, _)).run(x) == ((
          0.until(n1).map(_ + x),
          0.until(n2).map(n1 + _ + x)
        ), n1 + n2 + x)
    }

    val set = forAll(Gen.int ** Gen.int) {
      case (x, y) =>
        val p =
          stateMonad.flatMap(State.get[Int])(State.set(_)).run(x) == stateMonad
            .unit(())
            .run(x)
        val q = stateMonad
          .flatMap(State.set(x))(_ => State.get[Int])
          .run(y)
          ._1 == stateMonad.unit(x).run(y)._1
        p && q
    }

    Seq(rep, seq, map2, set).foreach(run(_))
  }

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(stateMonad[Int].unit(List.empty[(Int, A)])) {
      case (list, a) =>
        for {
          list <- list
          n <- State.get[Int]
          _ <- State.set(n + 1)
        } yield (n, a) :: list
    }.run(0)
      ._1
      .reverse

  case class Reader[R, A](run: R => A)

  object Reader {
    def get[R]: Reader[R, R] = Reader(r => r)

    def readerMonad[R] =
      new Monad[({ type T[A] = Reader[R, A] })#T] {
        def flatMap[A, B](a: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
          Reader { r =>
            f(a.run(r)).run(r)
          }

        def unit[A](a: => A): Reader[R, A] =
          Reader(_ => a)
      }
  }
}
