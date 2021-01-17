package fp_in_scala_exercises
import scala.annotation.tailrec

object ch6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val n = (nextSeed >>> 16).toInt
      (n, SimpleRNG(nextSeed))
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt

    if (n == Int.MinValue) {
      nonNegativeInt(rng2)
    } else {
      (Math.abs(n), rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    intDouble(rng) match {
      case ((n, d), rng2) => ((d, n), rng2)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (x, rng2) = double(rng);
    val (y, rng3) = double(rng2);
    val (z, rng4) = double(rng3);
    ((x, y, z), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def ints0(count: Int, list: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) {
        (list, rng)
      } else {
        val (n, rng2) = rng.nextInt
        ints0(count - 1, n :: list)(rng2)
      }
    }

    ints0(count, Nil)(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    s(_) match {
      case (a, rng) => (f(a), rng)
    }

  val nonNegativeEvenInt: Rand[Int] = {
    map(nonNegativeInt)(n => n - n % 2)
  }

  val double2: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def map2[A, B, C](s: Rand[A], t: Rand[B])(f: (A, B) => C): Rand[C] =
    s(_) match {
      case (a, rng) =>
        t(rng) match {
          case (b, rng2) => (f(a, b), rng2)
        }
    }

  def both[A, B](s: Rand[A], t: Rand[B]) = map2(s, t)((_, _))

  val intDouble2: Rand[(Int, Double)] = both(int, double2)

  def sequence[A](ss: List[Rand[A]]): Rand[List[A]] =
    rng =>
      ss.foldLeft((List.empty[A], rng)) {
        case ((list, rng), r) =>
          map(r)(_ :: list)(rng)
      }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))(_)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt(_) match {
      case (x, rng) =>
        if (x > Int.MaxValue - Int.MaxValue % n) {
          nonNegativeLessThan(n)(rng)
        } else {
          (x % n, rng)
        }
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    f(_) match {
      case (a, rng) => g(a)(rng)
    }

  def nonNegativeLessThan2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { x =>
      if (x > Int.MaxValue - Int.MaxValue % n) {
        nonNegativeLessThan2(n)
      } else {
        unit(x % n)
      }
    }

  def mapPrime[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Prime[A, B, C](s: Rand[A], t: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(s)(a => flatMap(t)(b => unit(f(a, b))))

  val rollDie: Rand[Int] = map(nonNegativeLessThan2(6))(_ + 1)

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(run(_) match {
        case (a, s) => (f(a), s)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(run(_) match {
        case (a, s) => f(a).run(s)
      })
  }

  object State {
    def unit[S, A](a: A) = State((s: S) => (a, s))

    def map2[S, A, B, C](s: State[S, A], t: State[S, B])(
        f: (A, B) => C
    ): State[S, C] =
      State(s.run(_) match {
        case (a, u) =>
          t.run(u) match {
            case (b, v) => (f(a, b), v)
          }
      })

    def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
      State(x =>
        ss.foldRight((List.empty[A], x)) {
          case (s, (list, y)) =>
            val (a, t) = s.run(y)
            (a :: list, t)
        }
      )

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: => S): State[S, Unit] = State(_ => ((), s))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State
      .sequence(inputs.map {
        case Coin =>
          State((m: Machine) =>
            if (m.locked && m.candies > 0) {
              ((), Machine(false, m.candies, m.coins + 1))
            } else {
              ((), m)
            }
          )
        case Turn =>
          State((m: Machine) =>
            if (!m.locked) {
              ((), Machine(true, Math.max(0, m.candies - 1), m.coins))
            } else {
              ((), m)
            }
          )
      })
      .flatMap(_ => State(m => ((m.coins, m.candies), m)));
}
