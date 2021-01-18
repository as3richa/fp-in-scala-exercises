package fp_in_scala_exercises
import ch5.Stream
import ch8.{Gen, Prop, run, forAll}
import ch11.Functor
import scala.annotation.tailrec
import java.util.Date
import java.text.SimpleDateFormat

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
    def flatMap[A, B](a: F[A])(f: A => F[B]) = join(map(a)(f))
    def join[A](a: F[F[A]]): F[A] = flatMap(a)(a => a)

    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g(_))

    def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
      flatMap(a) { a =>
        map(b)(f(a, _))
      }

    override def map[A, B](a: F[A])(f: A => B): F[B] =
      flatMap(a)(a => unit(f(a)))
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
}
