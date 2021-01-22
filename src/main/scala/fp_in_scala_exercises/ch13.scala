package fp_in_scala_exercises
import ch12.Monad
import scala.annotation.tailrec

object ch13 {
  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): IO[Unit] =
    printLine(message(p1, p2))

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) {
      Some(p1)
    } else if (p1.score < p2.score) {
      Some(p2)
    } else {
      None
    }

  def message(p1: Player, p2: Player): String =
    winner(p1, p2)
      .map(winner => s"${winner.name} is the winner!")
      .getOrElse("It's a draw.")

  trait IO[A] {
    def run: A

    def skip: IO[Unit] = IO { run }

    def **[B](io: IO[B]): IO[(A, B)] =
      IO.product(this, io)

    def >*[B](io: IO[B]): IO[B] =
      IO.map2(this, io)((_, b) => b)

    def map[B](f: A => B): IO[B] =
      flatMap(a => IO(f(a)))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO {
        f(run).run
      }
  }

  object IO extends Monad[IO] {
    def unit[A](f: => A): IO[A] = new IO[A] { def run: A = f }
    override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a.flatMap(f)
    def apply[A](f: => A): IO[A] = unit(f)
  }

  def printLine(msg: String): IO[Unit] = IO { println(msg) }
  def readLine: IO[String] = IO { scala.io.StdIn.readLine }

  def fahrenheitToCelsius(degF: Double): Double =
    (degF - 32) * 5.0 / 9.0

  def converter: IO[Unit] =
    for {
      _ <- printLine("Enter a temperature in degrees Fahrenheit")
      degC <- readLine.map(line => fahrenheitToCelsius(line.toDouble))
      _ <- printLine(degC.toString())
    } yield ()

  val echo: IO[Unit] = readLine.flatMap(printLine(_))
  val readInt: IO[Int] = readLine.map(_.toInt)
  val readInts: IO[List[Int]] = readLine.map(_.split(" ").map(_.toInt).toList)

  case class Ref[A](var a: A) extends IO[Unit] {
    def run: Unit = ()

    def modify(f: A => A): IO[Unit] = {
      a = f(a)
      this
    }

    def get: IO[A] = IO { a }
  }

  def ref[A](a: A): IO[Ref[A]] = IO.unit(Ref(a))

  def factorial(n: Long): Long = {
    @tailrec
    def factorial0(n: Long, v: Long = 1): Long =
      if (n <= 1) {
        v
      } else {
        factorial0(n - 1, n * v)
      }
    factorial0(n)
  }

  def factorial2(n: Int): IO[Int] =
    for {
      acc <- ref(1)
      _ <- IO.foreachM(1.to(n).toStream)(i => acc.modify(_ * i))
      fac <- acc.get
    } yield fac

  def PrintLines(msg: String*): IO[Unit] =
    msg.toList.foldLeft(IO.unit(())) {
      case (io, s) => io >* printLine(s)
    }

  val factorialRepl: IO[Unit] =
    PrintLines(
      "The Amazing Factorial REPL, v2.0",
      "q - quit",
      "<number> - compute the factorial of the given number",
      "<anything else> - crash spectacularly"
    ) >* IO.doWhile(readLine) { line =>
      IO.when(line != "q") {
        for {
          fac <- factorial2(line.toInt)
          _ <- printLine(fac.toString)
        } yield ()
      }
    }
}
