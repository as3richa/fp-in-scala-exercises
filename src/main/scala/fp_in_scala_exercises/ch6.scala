package fp_in_scala_exercises

import scala.annotation.tailrec
import fp_in_scala_exercises.ch6.Coin
import fp_in_scala_exercises.ch6.Turn

object ch6 {
  trait RNG { self =>
    def nextInt: (Int, RNG)

    @tailrec
    private def rerollWhile(f: Int => Boolean)(g: Rand[Int]): (Int, RNG) = {
      val (value, rng) = g(self)
      if (f(value)) {
        rng.rerollWhile(f)(g)
      } else {
        (value, rng)
      }
    }

    def map[A, B](f: Rand[A])(g: A => B): Rand[B] =
      f(_) match {
        case (a, rng) => (g(a), rng)
      }

    def map2[A, B, C](f: Rand[A])(g: Rand[B])(h: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = f(rng)
        val (b, rng3) = g(rng2)
        (h(a, b), rng3)
      }

    def both[A, B](f: Rand[A])(g: Rand[B]): Rand[(A, B)] =
      map2(f)(g)((_, _))

    def nonNegativeInt: (Int, RNG) =
      map(_.rerollWhile(_ == Int.MinValue)(_.nextInt))(Math.abs(_))(self)

    def double: (Double, RNG) =
      map(_.rerollWhile(_ == Int.MaxValue)(_.nonNegativeInt))(
        _.toDouble / Int.MaxValue
      )(self)

    def intDouble: ((Int, Double), RNG) = {
      val (int, rng) = nonNegativeInt
      val (double, rng2) = rng.double
      ((int, double), rng2)
    }

    def doubleInt: ((Double, Int), RNG) =
      intDouble match {
        case ((int, double), rng) => ((double, int), rng)
      }

    def double3: (Double, Double, Double) = {
      val (d, rng) = double
      val (d2, rng2) = rng.double
      val (d3, rng3) = rng2.double
      (d, d2, d3)
    }

    def ints(count: Int): (List[Int], RNG) =
      0.until(count).foldRight((List.empty[Int], self)) {
        case (_, (list, rng)) =>
          val (value, rng2) = rng.nextInt
          (value :: list, rng2)
      }

    def list[A](count: Int)(f: Rand[A]) =
      0.until(count).foldRight((List.empty[A], self)) {
        case (_, (list, rng)) =>
          val (value, rng2) = f(rng)
          (value :: list, rng2)
      }

    def ints2(count: Int): (List[Int], RNG) =
      list(count)(_.nextInt)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      rng => {
        fs.foldLeft((List.empty[A], rng)) {
          case ((list, rng), f) =>
            val (value, rng2) = f(rng)
            (value :: list, rng2)
        }
      }

    def intDouble2: ((Int, Double), RNG) =
      both(_.nextInt)(_.double)(self)

    def doubleInt2: ((Double, Int), RNG) =
      both(_.double)(_.nextInt)(self)

    def nonNegativeLessThan(n: Int): (Int, RNG) = {
      val limit = (Int.MaxValue / n) * n
      map(_.rerollWhile(_ >= limit)(_.nonNegativeInt))(_ % n)(self)
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng)
      }

    def map3[A, B](f: Rand[A])(g: A => B): Rand[B] =
      flatMap(f)(a => rng => (g(a), rng))

    def nonNegativeLessThan2(n: Int): (Int, RNG) = {
      val limit = (Int.MaxValue / n) * n
      flatMap(_.nonNegativeInt)(a =>
        if (a < limit) {
          (a, _)
        } else {
          _.nonNegativeLessThan2(n)
        }
      )(self)
    }
  }

  def unit[A](a: A): Rand[A] = (a, _)

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      ((nextSeed >>> 16).toInt, SimpleRNG(nextSeed))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  case class State[S, +A](run: S => (A, S)) {
    def map[B](g: A => B): State[S, B] =
      State { state =>
        val (a, next) = run(state)
        (g(a), next)
      }

    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State { state =>
        val (a, next) = run(state)
        g(a).run(next)
      }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def map2[S, A, B](f: State[S, A])(g: State[S, B]): State[S, (A, B)] =
      State { state =>
        val (a, state2) = f.run(state)
        val (b, state3) = g.run(state2)
        ((a, b), state3)
      }

    def sequence[S, B](fs: List[State[S, B]]): State[S, List[B]] =
      State { state =>
        fs.foldLeft((List.empty[B], state)) {
          case ((list, state), f) =>
            val (b, state2) = f.run(state)
            (b :: list, state2)
        }
      }

    def get[S]: State[S, S] = State { state => (state, state) }

    def set[S](state: S): State[S, Unit] = State { _ => ((), state) }

    def modify[S](f: S => S): State[S, Unit] = get.flatMap((s: S) => set(f(s)))

    def modify2[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(s)
      } yield ()
  }

  def map[S, A, B](f: S => (A, S))(g: A => B)(state: S): (B, S) = {
    val (a, next) = f(state)
    (g(a), next)
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int) { self =>
    def apply(input: Input): Machine =
      input match {
        case Coin if locked && candies > 0 => self.copy(locked = false)
        case Turn if !locked && candies > 0 =>
          Machine(locked = true, candies = candies - 1, coins = coins + 1)
        case _ => self
      }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State {
      State
        .sequence(
          inputs.map(input =>
            State[Machine, Unit] { machine => ((), machine.apply(input)) }
          )
        )
        .flatMap(_ =>
          State((machine: Machine) =>
            ((machine.coins, machine.candies), machine)
          )
        )
        .run(_)
    }
}
