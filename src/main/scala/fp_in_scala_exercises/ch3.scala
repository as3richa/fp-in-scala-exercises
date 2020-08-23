package fp_in_scala_exercises

object ch3 {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int =
      ints match {
        case Nil              => 0
        case Cons(head, tail) => head + sum(tail)
      }

    def product(ds: List[Double]): Double =
      ds match {
        case Nil              => 1.0
        case Cons(head, tail) => head * product(tail)
      }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty)
        Nil
      else
        Cons(as.head, apply(as.tail: _*))

    def tail[A](as: List[A]): List[A] =
      as match {
        case Nil           => throw new RuntimeException("can't take tail of Nil")
        case Cons(_, tail) => tail
      }

    def setHead[A](head: A, as: List[A]): List[A] = Cons(head, tail(as))

    def drop[A](as: List[A], n: Int): List[A] =
      if (n == 0) {
        as
      } else {
        as match {
          case Nil           => Nil
          case Cons(_, tail) => drop(tail, n - 1)
        }
      }

    def dropWhile0[A](as: List[A], f: A => Boolean): List[A] =
      as match {
        case Nil => Nil
        case Cons(head, tail) =>
          if (f(head)) {
            dropWhile0(tail, f)
          } else {
            as
          }
      }

    def append[A](as: List[A], other: List[A]): List[A] =
      as match {
        case Nil              => other
        case Cons(head, tail) => Cons(head, append(tail, other))
      }

    def init[A](as: List[A]): List[A] =
      as match {
        case Nil              => Nil
        case Cons(head, Nil)  => Nil
        case Cons(head, tail) => Cons(head, init(tail))
      }

    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Nil => Nil
        case Cons(head, tail) =>
          if (f(head)) {
            dropWhile(tail)(f)
          } else {
            as
          }
      }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil              => z
        case Cons(head, tail) => f(head, foldRight(tail, z)(f))
      }

    def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

    def product2(ds: List[Double]): Double = foldRight(ds, 0.0)(_ * _)

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, length) => length + 1)

    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil              => z
        case Cons(head, tail) => foldLeft(tail, f(head, z))(f)
      }

    def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def length2[A](as: List[A]): Int =
      foldLeft(as, 1)((_, length) => length + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft[A, List[A]](as, Nil)(Cons(_, _))

    def foldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      val reversed = foldRight[A, List[A]](as, Nil)((a, reversed) =>
        append(reversed, apply(a))
      )
      foldRight[A, B](reversed, z)(f)
    }

    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)(f)

    def append2[A](as: List[A], other: List[A]): List[A] =
      foldRight(as, other)(Cons(_, _))

    def flatten[A](lists: List[List[A]]): List[A] =
      foldRight[List[A], List[A]](lists, Nil)(append(_, _))

    def mapPlusOne(ints: List[Int]): List[Int] =
      foldRight[Int, List[Int]](ints, Nil)((head, mapped) =>
        Cons(head + 1, mapped)
      )

    def mapDoubleToString(ds: List[Double]): List[String] =
      foldRight[Double, List[String]](ds, Nil)((head, mapped) =>
        Cons(head.toString(), mapped)
      )

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight[A, List[B]](as, Nil)((head, mapped) => Cons(f(head), mapped))

    def flatMap[A, B](as: List[A])(f: A => List[B]) =
      foldRight[A, List[B]](as, Nil)((head, mapped) => append(f(head), mapped))

    def filter[A](as: List[A])(f: A => Boolean) =
      flatMap(as)(a => if (f(a)) apply(a) else Nil)

    def zipAdd(ints: List[Int], other: List[Int]): List[Int] =
      (ints, other) match {
        case (Cons(head1, tail1), Cons(head2, tail2)) =>
          Cons(head1 + head2, zipAdd(tail1, tail2))
        case (Cons(_, _), Nil) => Nil
        case (Nil, Cons(_, _)) => Nil
        case (Nil, Nil)        => Nil
      }

    def zipWith[A, B, C](as: List[A], bs: List[B])(fn: (A, B) => C): List[C] =
      (as, bs) match {
        case (Cons(a, tail1), Cons(b, tail2)) =>
          Cons(fn(a, b), zipWith(tail1, tail2)(fn))
        case (Cons(_, _), Nil) => Nil
        case (Nil, Cons(_, _)) => Nil
        case (Nil, Nil)        => Nil
      }

    def hasSubsequence[A](as: List[A], query: List[A]): Boolean =
      (as, query) match {
        case (Cons(head, tail), Cons(qHead, qTail)) =>
          if (head == qHead)
            hasSubsequence(tail, qTail)
          else
            hasSubsequence(tail, query)
        case (Nil, Cons(_, _)) => false
        case (_, Nil)          => true
      }

    def startsWith[A](as: List[A], query: List[A]): Boolean =
      (as, query) match {
        case (_, Nil) => true
        case (Cons(a1, tail1), Cons(a2, tail2)) =>
          a1 == a2 && startsWith(tail1, tail2)
        case (_, _) => false
      }

    def hasSubstring[A](as: List[A], query: List[A]): Boolean =
      startsWith(as, query) || (as match {
        case (Cons(_, tail)) => hasSubstring(tail, query)
        case Nil             => false
      })
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int =
      tree match {
        case Leaf(_)             => 1
        case Branch(left, right) => size(left) + size(right)
      }

    def maximum(tree: Tree[Int]): Int =
      tree match {
        case Leaf(value)         => value
        case Branch(left, right) => maximum(left).max(maximum(right))
      }

    def depth(tree: Tree[Int]): Int =
      tree match {
        case Leaf(_)             => 0
        case Branch(left, right) => 1 + depth(left).max(depth(right))
      }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(value)         => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
      tree match {
        case Leaf(value)         => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      }

    def size2[A](tree: Tree[A]) = fold(tree)(_ => 1)(_ + _)

    def maximum2(tree: Tree[Int]) = fold(tree)(identity)(_.max(_))

    def depth2[A](tree: Tree[A]) = fold(tree)(_ => 0)(_.max(_) + 1)

    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](tree)(value => Leaf(f(value)))(Branch(_, _))
  }
}
