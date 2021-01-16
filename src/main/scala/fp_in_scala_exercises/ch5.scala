package fp_in_scala_exercises
import scala.annotation.tailrec

object ch5 {
  def if2[A](cond: Boolean, yes: => A, no: => A) =
    if (cond)
      yes
    else
      no

  sealed trait Stream[+A] {
    def headOption: Option[A] =
      this match {
        case Empty            => None
        case Cons(head, tail) => Some(head())
      }

    def toList: List[A] =
      this match {
        case Empty            => Nil
        case Cons(head, tail) => head() :: tail().toList
      }

    def take(n: Int): Stream[A] =
      if (n <= 0) Empty
      else
        this match {
          case Empty => Empty
          case Cons(head, tail) =>
            lazy val tl = tail().take(n - 1)
            Cons(head, () => tl)
        }

    def drop(n: Int): Stream[A] =
      if (n <= 0) this
      else
        this match {
          case Empty            => Empty
          case Cons(head, tail) => tail().drop(n - 1)
        }

    def takeWhile(p: A => Boolean): Stream[A] =
      this match {
        case Empty => Empty
        case Cons(head, tail) =>
          if (p(head())) {
            lazy val tl = tail().takeWhile(p)
            Cons(head, () => tl)
          } else {
            Empty
          }
      }

    def exists(p: A => Boolean): Boolean =
      this match {
        case Empty            => false
        case Cons(head, tail) => p(head()) || tail().exists(p)
      }

    def foldLeft[B](z: => B)(f: (=> B, A) => B): B = {
      @tailrec
      def foldLeft0(s: Stream[A], z: => B): B =
        s match {
          case Empty            => z
          case Cons(head, tail) => foldLeft0(tail(), f(z, head()))
        }
      foldLeft0(this, z)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Empty            => z
        case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      }

    def exists2(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((head, tail) =>
        if (p(head)) {
          Stream.cons(head, tail)
        } else {
          Empty
        }
      )

    def headOption2: Option[A] =
      foldRight(Option.empty[A])((head, tail) => Some(head))

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((head, tail) => Stream.cons(f(head), tail))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((head, tail) =>
        if (p(head)) {
          Stream.cons(head, tail)
        } else {
          tail
        }
      )

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)(Stream.cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((head, tail) => f(head).append(tail))

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    def map2[B](f: A => B): Stream[B] =
      Stream.unfold(this) {
        case Cons(head, tail) => Some((f(head()), tail()))
        case Empty            => None
      }

    def take2(n: Int): Stream[A] =
      Stream.unfold((this, n)) {
        case (s, n) =>
          s match {
            case Cons(head, tail) if n > 0 => Some((head(), (tail(), n - 1)))
            case _                         => None
          }
      }

    def takeWhile3(f: A => Boolean): Stream[A] =
      Stream.unfold(this) {
        case Cons(head, tail) if f(head()) => Some((head(), tail()))
        case _                             => None
      }

    def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _                            => None
      }

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s)) {
        case (r, s) =>
          def unpack[C](x: Stream[C]) =
            x match {
              case Cons(head, tail) => (Some(head()), tail())
              case Empty            => (None, Empty)
            }

          val (rv, rs) = unpack(r)
          val (sv, ss) = unpack(s)

          if (rv.isEmpty && sv.isEmpty)
            None
          else
            Some(((rv, sv), (rs, ss)))
      }

    def startsWith[B](s: Stream[B]): Boolean =
      this.zipAll(s).takeWhile(_._2.isDefined).forAll {
        case (left, right) => left == right
      }

    def tails: Stream[Stream[A]] =
      Stream.unfold(Option(this)) {
        _.map { s =>
          s match {
            case Cons(_, tail) => (s, Some(tail()))
            case Empty         => (s, None)
          }
        }
      }

    def hasSubsequence[B](s: Stream[B]): Boolean =
      tails.exists(_.startsWith(s))

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z))) {
        case (a, (b, s)) =>
          lazy val c = f(a, b)
          (c, Cons(() => c, () => s))
      }._2
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
      lazy val hd = head
      lazy val tl = tail
      Cons(() => hd, () => tl)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      as.foldRight(empty[A])(cons(_, _))

    val ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] =
      Cons(() => a, () => constant(a))

    def from(n: Int): Stream[Int] =
      Cons(() => n, () => from(n + 1))

    def fibs: Stream[Int] = {
      def fibs0(x: Int, y: Int): Stream[Int] =
        Cons(() => x, () => fibs0(y, x + y))
      fibs0(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z)
        .map { case (a, s) => Cons(() => a, () => unfold(s)(f)) }
        .getOrElse(Empty)

    def fibs2: Stream[Int] =
      unfold((0, 1)) { case (x, y) => Some((x, (y, x + y))) }

    def ones2: Stream[Int] = unfold(())(_ => Some((1, ())))

    def constant2[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))

    def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))
  }
}
