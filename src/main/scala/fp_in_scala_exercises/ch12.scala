package fp_in_scala_exercises
import ch5.Stream
import ch8.{Gen, Prop, run, forAll}
import ch10.Foldable
import ch11.{Functor, idMonad}
import scala.annotation.tailrec
import java.util.Date
import java.text.SimpleDateFormat
import _root_.fp_in_scala_exercises.ch10.Monoid
import _root_.fp_in_scala_exercises.ch6.State
import scala.collection.immutable.Stream.Cons
import scala.collection.immutable.Stream.Empty

object ch12 {
  trait Applicative[F[_]] extends Functor[F] { self =>
    def unit[A](a: => A): F[A]
    def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C]

    def map[A, B](a: F[A])(f: A => B): F[B] =
      map2(a, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, bs) => map2(f(a), bs)(_ :: _))

    def sequence[A](as: List[F[A]]): F[List[A]] =
      traverse(as)(a => a)

    def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] =
      m.foldRight(unit(Map.empty[K, V])) {
        case ((k, v), m) =>
          map2(m, v) { (m, v) =>
            m.updated(k, v)
          }
      }

    def replicateM[A](n: Int, a: F[A]): F[List[A]] = {
      @tailrec
      def replicateM0(n: Int, as: F[List[A]] = unit(Nil)): F[List[A]] =
        if (n <= 0) {
          as
        } else {
          replicateM0(n - 1, map2(a, as)(_ :: _))
        }
      replicateM0(n)
    }

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
      as.foldRight(unit(List.empty[A])) { (a, as) =>
        map2(f(a), as) { (keep, as) =>
          if (keep) {
            a :: as
          } else {
            as
          }
        }
      }

    def apply[A, B](f: F[A => B])(a: F[A]): F[B] =
      map2(f, a)(_(_))

    def applyMap[A, B](a: F[A])(f: A => B): F[B] = apply(unit(f(_)))(a)

    def applyMap2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(a))(b)

    def map3[A, B, C, D](a: F[A], b: F[B], c: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(a))(b))(c)

    def map4[A, B, C, D, E](a: F[A], b: F[B], c: F[C], d: F[D])(
        f: (A, B, C, D) => E
    ): F[E] =
      apply(apply(apply(apply(unit(f.curried))(a))(b))(c))(d)

    def product[A, B](a: F[A], b: F[B]): F[(A, B)] = map2(a, b)((_, _))

    def productMap2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C) =
      map(product(a, b)) { case (a, b) => f(a, b) }

    def product[G[_]](
        g: Applicative[G]
    ): Applicative[({ type T[A] = (F[A], G[A]) })#T] =
      new Applicative[({ type T[A] = (F[A], G[A]) })#T] {
        def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))

        def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(
            f: (A, B) => C
        ): (F[C], G[C]) = (self.map2(a._1, b._1)(f), g.map2(a._2, b._2)(f))
      }

    def compose[G[_]](
        g: Applicative[G]
    ): Applicative[({ type T[A] = F[G[A]] })#T] =
      new Applicative[({ type T[A] = F[G[A]] })#T] {
        def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))

        def map2[A, B, C](a: F[G[A]], b: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(a, b) { (a, b) =>
            g.map2(a, b) { (a, b) =>
              f(a, b)
            }
          }
      }

    trait AppEq {
      def apply[A](x: F[A], y: F[A]): Boolean
    }

    val defaultEq = new AppEq {
      def apply[A](x: F[A], y: F[A]): Boolean = x == y
    }

    case class Laws(eq: AppEq = defaultEq) {
      def map2Identity[A](a: Gen[F[A]]): Prop =
        forAll(a) { a =>
          eq(map2(a, unit(()))((a, _) => a), a) &&
          eq(map2(unit(()), a)((_, a) => a), a)
        }

      def map2Associativity[A, B, C](a: Gen[F[A]], b: Gen[F[B]], c: Gen[F[C]]) =
        forAll(a ** b ** c) {
          case ((a, b), c) =>
            eq(
              product(product(a, b), c),
              (map(product(a, product(b, c))) {
                case (a, (b, c)) => ((a, b), c)
              })
            )
        }

      def map2Naturality[A, B, C, D](a: Gen[F[A]], b: Gen[F[B]])(
          f: A => C
      )(g: B => D) =
        forAll(a ** b) {
          case (a, b) =>
            val fg = (a: A, b: B) => {
              (f(a), g(b))
            }
            eq(map2(a, b)(fg), product(map(a)(f), map(b)(g)))
        }
    }
  }

  trait Monad[F[_]] extends Applicative[F] {
    case class MonadOps[A](as: F[A], m: Monad[F]) {
      def flatMap[B](f: A => F[B]): F[B] = m.flatMap(as)(f)
      def map[B](f: A => B): F[B] = m.map(as)(f)
      def skip: F[Unit] = m.skip(as)
    }

    implicit def toMonadOps[A](as: F[A]): MonadOps[A] =
      MonadOps(as, this)

    def flatMap[A, B](a: F[A])(f: A => F[B]): F[B] = join(map(a)(f))
    def join[A](a: F[F[A]]): F[A] = flatMap(a)(a => a)

    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g(_))

    def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
      flatMap(a) { a =>
        map(b)(f(a, _))
      }

    override def map[A, B](a: F[A])(f: A => B): F[B] =
      flatMap(a)(a => unit(f(a)))

    def foreachM[A](
        as: scala.collection.immutable.Stream[A]
    )(f: A => F[Unit]): F[Unit] =
      foldM_(as)(())((_, a) => f(a))

    def doWhile[A](as: F[A])(f: A => F[Boolean]): F[Unit] =
      for {
        a <- as
        cond <- f(a)
        _ <-
          if (cond) {
            doWhile(as)(f)
          } else {
            unit(())
          }
      } yield ()

    def forever[A, B](as: F[A]): F[B] =
      for {
        a <- as
        b <- forever[A, B](as)
      } yield (b)

    def foldM[A, B](as: scala.collection.immutable.Stream[A])(z: B)(
        f: (B, A) => F[B]
    ): F[B] =
      as match {
        case a #:: tail => f(z, a).flatMap(foldM(tail)(_)(f))
        case _          => unit(z)
      }

    def foldM_[A, B](
        as: scala.collection.immutable.Stream[A]
    )(z: B)(f: (B, A) => F[B]): F[Unit] =
      foldM(as)(z)(f(_, _)).skip

    def skip[A](as: F[A]): F[Unit] =
      as.map(_ => ())

    def when[A](cond: Boolean)(f: => F[Unit]): F[Boolean] =
      if (cond) {
        f.map(_ => true)
      } else {
        unit(false)
      }
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] =
      a.flatMap(f)
    def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.constant(a)

    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.zipWith(b)(f(_, _))
  }

  def testStreamSequence: Unit = {
    run(Prop.check {
      streamApplicative.sequence(Nil).take(100).forAll(_.isEmpty)
    })

    val g =
      Gen.int.listOfN(Gen.choose(0, 100)).listOfN(Gen.choose(1, 10))

    run(forAll(g) { lists =>
      val expectation = lists
        .foldRight(Option.empty[List[List[Int]]]) {
          case (list, None) =>
            Some(list.map(List(_)))
          case (list, Some(lists)) =>
            Some(list.zip(lists).map {
              case (x, xs) => x :: xs
            })
        }
        .getOrElse(List())

      streamApplicative
        .sequence(lists.map(list => Stream.apply(list: _*)))
        .toList == expectation
    })
  }

  case class Id[A](get: A)

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a.get)
    def unit[A](a: => A): Id[A] = Id(a)
  }

  val idApplicative = new Applicative[Id] {
    def map2[A, B, C](a: Id[A], b: Id[B])(f: (A, B) => C): Id[C] =
      Id(f(a.get, b.get))

    def unit[A](a: => A): Id[A] = Id(a)
  }

  def eitherMonad[E]: Monad[({ type T[A] = Either[E, A] })#T] =
    new Monad[({ type T[A] = Either[E, A] })#T] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](
          a: Either[E, A]
      )(f: A => Either[E, B]): Either[E, B] =
        a match {
          case Right(a) => f(a)
          case Left(e)  => Left(e)
        }
    }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
      extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]
      : Applicative[({ type T[A] = Validation[E, A] })#T] =
    new Applicative[({ type T[A] = Validation[E, A] })#T] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](
          a: Validation[E, A],
          b: Validation[E, B]
      )(f: (A, B) => C): Validation[E, C] =
        (a, b) match {
          case (Success(a), Success(b))          => Success(f(a, b))
          case (Success(_), Failure(head, tail)) => Failure(head, tail)
          case (Failure(head, tail), Success(_)) => Failure(head, tail)
          case (Failure(lhead, ltail), Failure(rhead, rtail)) =>
            Failure(lhead, (ltail :+ rhead) ++ rtail)
        }
    }

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  object WebForm {
    def validated(
        name: String,
        birthdate: String,
        phoneNumber: String
    ): Validation[String, WebForm] = {
      val validateName =
        if (name.isEmpty) {
          Failure("Name must not be empty")
        } else {
          Success(name)
        }

      val validateBirthdate = {
        val date = if (birthdate.matches("^[0-9]{4}-[0-9]{2}-[0-9]{2}$")) {
          Option(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
        } else {
          None
        }
        date
          .map(Success(_))
          .getOrElse(Failure("Birth date must be of the form yyyy-MM-dd"))
      }

      val validatePhoneNumber =
        if (phoneNumber.matches("^[0-9]{10}$")) {
          Success(phoneNumber)
        } else {
          Failure("Phone number must be 10 digits")
        }

      validationApplicative.map3(
        validateName,
        validateBirthdate,
        validatePhoneNumber
      )(WebForm(_, _, _))
    }
  }

  def testValidation: Unit = {
    run(Prop.check {
      WebForm.validated("adam", "1995-12-14", "1234567890") match {
        case Success(WebForm(name, birthdate, phoneNumber)) =>
          (
            name == "adam" &&
              new SimpleDateFormat("yyyy-MM-dd").format(
                birthdate
              ) == "1995-12-14" &&
              phoneNumber == "1234567890"
          )
        case Failure(head, tail) => false
      }
    })

    run(Prop.check {
      WebForm.validated("", "1995-12-14x", "1234567890x") == Failure(
        "Name must not be empty",
        Vector(
          "Birth date must be of the form yyyy-MM-dd",
          "Phone number must be 10 digits"
        )
      )
    })
  }

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(a => b.map(f(a, _)))
  }

  case class Employee(name: String, id: Int)
  case class Pay(rate: Double, hoursPerYear: Double)

  def formatPay(name: Option[String], pay: Option[Double]): Option[String] =
    optionApplicative.map2(name, pay) { (name, pay) =>
      s"${name} makes ${pay}"
    }

  def formatEmployeePay(e: Option[Employee], pay: Option[Pay]): Option[String] =
    formatPay(e.map(_.name), pay.map(pay => pay.rate * pay.hoursPerYear))

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
    def map[A, B](as: F[A])(f: A => B): F[B] =
      traverse(as)(a => idApplicative.unit(f(a)))(idApplicative).get

    def traverse[G[_], A, B](
        as: F[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = {
      sequence(map(as)(f))
    }

    def sequence[G[_], A](as: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
      traverse(as)(a => a)

    override def foldMap[A, B](
        as: F[A]
    )(f: A => B)(B: Monoid[B]): B = {
      type G[A] = B

      val app = new Applicative[G] {
        def unit[X](x: => X): B = B.zero

        def map2[X, Y, Z](x: B, y: B)(f: (X, Y) => Z): B = B.op(x, y)
      }

      traverse[G, A, B](as)(f)(app)
    }

    def traverseS[S, A, B](as: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse(as)(f)(stateApplicative)

    def mapAccum[S, A, B](as: F[A])(z: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(as)(a => State(f(a, _))).run(z)

    def zipWithIndex[A](as: F[A]): F[(A, Int)] =
      mapAccum(as)(0)((a, i) => ((a, i), i + 1))._1

    override def toList[A](as: F[A]): List[A] =
      mapAccum(as)(List.empty[A])((a, list) => ((), a :: list))._2.reverse

    def zipWithList[A, B, C](as: F[A], bs: List[B])(f: (A, B) => C): F[C] =
      mapAccum(as)(bs)((a, bs) => (f(a, bs.head), bs.tail))._1

    def reverse[A](as: F[A]): F[A] = {
      val reversedList =
        mapAccum(as)(List.empty[A])((a, list) => ((), a :: list))._2
      zipWithList(as, reversedList)((_, b) => b)
    }

    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(as)(z)((a, b) => ((), f(b, a)))._2

    def zipL[A, B](as: F[A], bs: F[B]): F[(A, Option[B])] =
      mapAccum(as)(toList(bs))((a, list) =>
        ((a, list.headOption), list.drop(1))
      )._1

    def zipR[A, B](as: F[A], bs: F[B]): F[(Option[A], B)] =
      mapAccum(bs)(toList(as))((b, list) =>
        ((list.headOption, b), list.drop(1))
      )._1

    def fuse[G[_], H[_], A, B](as: F[A])(f: A => G[B], g: A => H[B])(implicit
        G: Applicative[G],
        H: Applicative[H]
    ): (G[F[B]], H[F[B]]) = {
      type T[A] = (G[A], H[A])
      traverse[T, A, B](as)(a => (f(a), g(a)))(G.product(H))
    }

    def compose[G[_], A](implicit
        G: Traverse[G]
    ): Traverse[({ type T[A] = F[G[A]] })#T] =
      new Traverse[({ type T[A] = F[G[A]] })#T] {
        override def traverse[H[_], A, B](
            as: F[G[A]]
        )(f: A => H[B])(implicit H: Applicative[H]): H[F[G[B]]] =
          self.traverse(as)(as => G.traverse(as)(f(_)))
      }
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Option.empty[List[B]]) {
      case (a, list) => list.flatMap(list => f(a).map(_ :: list))
    }

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)

    override def traverse[G[_], A, B](
        as: List[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List.empty[B])) { (a, bs) =>
        G.map2(f(a), bs)(_ :: _)
      }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](as: Option[A])(f: A => B): Option[B] =
      as match {
        case Some(a) => Some(f(a))
        case None    => None
      }

    override def sequence[G[_], A](as: Option[G[A]])(implicit
        G: Applicative[G]
    ): G[Option[A]] = as.map(g => G.map(g)(Option(_))).getOrElse(G.unit(None))
  }

  case class Tree[+A](head: A, tail: List[Tree[A]] = Nil)

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](as: Tree[A])(f: A => B): Tree[B] =
      as match {
        case Tree(head, tail) => Tree(f(head), tail.map(child => map(child)(f)))
      }

    override def traverse[G[_], A, B](
        as: Tree[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      as match {
        case Tree(head, tail) =>
          G.map2(
            f(head),
            listTraverse.traverse(tail.map(traverse(_)(f)))(x => x)
          )(Tree(_, _))
      }
  }

  def mapTraverse[K]: Traverse[({ type T[V] = Map[K, V] })#T] =
    new Traverse[({ type T[V] = Map[K, V] })#T] {
      override def map[A, B](as: Map[K, A])(f: A => B): Map[K, B] =
        as.map { case (k, a) => (k, f(a)) }

      override def sequence[G[_], A](as: Map[K, G[A]])(implicit
          G: Applicative[G]
      ): G[Map[K, A]] =
        as.foldRight(G.unit(Map.empty[K, A])) {
          case ((k, a), m) => G.map2(a, m)((a, m) => m.updated(k, a))
        }
    }

  def stateApplicative[S]: Applicative[({ type T[A] = State[S, A] })#T] =
    new Applicative[({ type T[A] = State[S, A] })#T] {
      def map2[A, B, C](a: State[S, A], b: State[S, B])(
          f: (A, B) => C
      ): State[S, C] =
        a.flatMap(a => b.map(b => f(a, b)))

      def unit[A](a: => A): State[S, A] =
        State.unit(a)
    }

  def testToList: Unit =
    run(forAll(Gen.int.listOfN(Gen.choose(0, 100))) { list =>
      listTraverse.toList(list) == list
    })

  def testZipWithIndex: Unit =
    run(forAll(Gen.int.listOfN(Gen.choose(0, 100))) { list =>
      listTraverse.zipWithIndex(list) == list.zip(0.until(list.length))
    })

  def testReverse: Unit = {
    val g = Gen.int.listOfN(Gen.choose(0, 100))

    run(forAll(g ** g ** g) {
      case ((x, y), z) =>
        listTraverse.reverse(y) ++ listTraverse.reverse(x) == listTraverse
          .reverse(x ++ y)
    })
  }

  def testZips: Unit = {
    val g = Gen.pair(Gen.choose(0, 5)).flatMap {
      case (n, m) =>
        Gen.listOfN(n, Gen.int) ** Gen.listOfN(n + m, Gen.int)
    }

    run(forAll(g) {
      case (small, big) =>
        listTraverse.zipL(small, big) == small.zip(big.map(Some(_))) &&
          listTraverse
            .zipL(big, small) == big.zipAll(small.map(Some(_)), -1, None) &&
          listTraverse
            .zipR(small, big) == small.map(Some(_)).zipAll(big, None, -1) &&
          listTraverse.zipR(big, small) == big.map(Some(_)).zip(small)
    })
  }

  def composeM[F[_], G[_]](
      F: Monad[F],
      G: Monad[G],
      H: Traverse[G]
  ): Monad[({ type T[A] = F[G[A]] })#T] =
    new Monad[({ type T[A] = F[G[A]] })#T] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def flatMap[A, B](a: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(a)(a => F.map(H.traverse(a)(a => f(a))(F))(G.join(_)))
    }

  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
      OptionT(M.flatMap(value) {
        case Some(a) => f(a).value
        case None    => M.unit(None)
      })
  }
}
