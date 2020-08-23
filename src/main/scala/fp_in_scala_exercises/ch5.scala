package fp_in_scala_exercises
import fp_in_scala_exercises.ch5.Empty
import fp_in_scala_exercises.ch5.Cons
import scala.annotation.tailrec

object ch5 {
  sealed trait Stream[+A] { self =>
    def headOption: Option[A]
    def foldLeft[B](z: B)(f: (A, B) => B): B
    def foldRight[B](z: B)(f: (A, => B) => B): B
    def drop(n: Int): Stream[A]
    def take(n: Int): Stream[A]
    def takeWhile(f: A => Boolean): Stream[A]
    def toList: List[A] = foldRight(List.empty[A])(_ :: _)

    def exists(f: A => Boolean): Boolean =
      foldRight(false)((a, found) => f(a) || found)

    def takeWhile2(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a, stream) =>
        if (f(a)) Stream.cons(a, stream) else Empty
      )

    def forAll(f: A => Boolean) = !exists(!f(_))

    def headOption2: Option[A] = foldRight(Option.empty[A])((a, opt) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, stream) => Stream.cons(f(a), stream))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a, stream) =>
        if (f(a)) {
          Stream.cons(a, stream)
        } else {
          stream
        }
      }

    def append[B >: A](other: => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B]) { (a, stream) =>
        Stream.cons[B](
          a,
          stream match {
            case Cons(_, _) => stream
            case Empty      => other
          }
        )
      }

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, stream) => f(a).append(stream))

    def find(f: A => Boolean): Option[A] =
      filter(f).headOption

    def map2[B](f: A => B): Stream[B] =
      Stream.unfold(self) {
        case Cons(head, tail) => Some((f(head()), tail()))
        case Empty            => None
      }

    def take2(n: Int): Stream[A] =
      Stream.unfold((self, n)) {
        case (Cons(head, tail), remaining) if remaining > 0 =>
          Some((head(), (tail(), n - 1)))
        case _ => None
      }

    def takeWhile3(f: A => Boolean) =
      Stream.unfold(self) {
        case Cons(head, tail) if f(head()) =>
          Some((head(), tail()))
        case _ => None
      }

    def zipWith[B, C](other: Stream[B])(f: (A, B) => C) =
      Stream.unfold((self, other)) {
        case (Cons(head, tail), Cons(otherHead, otherTail)) =>
          Some((f(head(), otherHead()), (tail(), otherTail())))
        case (_, _) => None
      }

    def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((self, other)) {
        case (me, other) =>
          def toPairOpt[C](stream: Stream[C]): Option[(C, Stream[C])] =
            stream match {
              case Empty            => None
              case Cons(head, tail) => Some((head(), tail()))
            }

          ((toPairOpt(me), toPairOpt(other))) match {
            case (Some((head, tail)), Some((otherHead, otherTail))) =>
              Some(((Some(head), Some(otherHead)), (tail, otherTail)))
            case (Some((head, tail)), None) =>
              Some(((Some(head), None), (tail, Empty)))
            case (None, Some((head, tail))) =>
              Some(((None, Some(head)), (Empty, tail)))
            case (None, None) => None
          }
      }

    def startsWith[B >: A](other: Stream[B]): Boolean =
      zipAll(other).takeWhile(_._2.isDefined).forAll {
        case (left, right) => left == right
      }

    def tails(): Stream[Stream[A]] =
      Stream.unfold(self) {
        case Cons(_, tail) => Some(tail(), tail())
        case Empty         => None
      }

    def isSubstring[B >: A](sub: Stream[B]): Boolean =
      tails.exists(_.startsWith(sub))

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight[Cons[B]](Cons(() => z, () => Empty)) {
        case (a, results) =>
          val b = f(a, results.head())
          Cons(() => b, () => results)
      }
  }

  case object Empty extends Stream[Nothing] {
    override def headOption: Option[Nothing] = None
    override def foldLeft[B](z: B)(f: (Nothing, B) => B): B = z
    override def foldRight[B](z: B)(f: (Nothing, => B) => B): B = z
    override def drop(n: Int) = Empty
    override def take(n: Int) = Empty
    override def takeWhile(f: Nothing => Boolean): Stream[Nothing] = Empty
    override def exists(f: Nothing => Boolean): Boolean = false
  }

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {
    self =>
    override def headOption: Option[A] = Some(head())

    override def foldLeft[B](z: B)(f: (A, B) => B): B =
      tail().foldLeft(f(head(), z))(f)

    override def foldRight[B](z: B)(f: (A, => B) => B): B =
      f(head(), tail().foldRight(z)(f))

    override def drop(n: Int) =
      if (n > 0) {
        tail().drop(n - 1)
      } else {
        self
      }

    override def take(n: Int): Stream[A] =
      if (n > 0) {
        Cons(head, () => tail().take(n - 1))
      } else {
        Empty
      }

    override def takeWhile(f: A => Boolean): Stream[A] =
      if (f(head())) {
        Cons(head, () => tail().takeWhile(f))
      } else {
        Empty
      }

    override def exists(f: A => Boolean): Boolean =
      f(head()) || tail().exists(f)
  }

  object Stream {
    def cons[A](headEx: => A, tailEx: => Stream[A]): Stream[A] = {
      lazy val head = headEx
      lazy val tail = tailEx
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = as.foldRight(empty[A])(cons(_, _))

    def constant[A](a: A): Stream[A] = {
      lazy val stream: Stream[A] = cons(a, stream)
      stream
    }

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def go(prev: Int, value: Int): Stream[Int] =
        cons(value, go(value, value + prev))
      go(1, 0)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z)
        .map { case (a, state) => cons(a, unfold(state)(f)) }
        .getOrElse(Empty)

    def fibs2(): Stream[Int] =
      unfold((1, 0)) {
        case (prev, value) =>
          Some(value, (value, prev + value))
      }

    def from2(n: Int): Stream[Int] =
      unfold(n)(value => Some((value, value + 1)))

    def constant2[A](a: A): Stream[A] = unfold(())(_ => Some(a, ()))

    val ones: Stream[Int] = constant2(1)
  }
}
