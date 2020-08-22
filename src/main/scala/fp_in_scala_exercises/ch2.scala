package fp_in_scala_exercises

import scala.annotation.tailrec

object ch2 {
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, x: Int, y: Int): Int =
      n match {
        case 1 => x
        case 2 => y
        case _ => go(n - 1, y, x + y)
      }
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length) {
        true
      } else {
        ordered(as(n - 1), as(n)) && go(n + 1)
      }
    }
    go(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => (b => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = curry(f)(a)
}
