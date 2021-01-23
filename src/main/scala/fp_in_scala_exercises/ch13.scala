package fp_in_scala_exercises
import ch7.nonblocking.Par
import ch12.Monad
import ch13.TailRec.Return
import java.util.concurrent.ExecutorService
import scala.annotation.tailrec
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Path, OpenOption}
import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption
import java.nio.file.FileSystems
import ch8.Gen
import java.util.concurrent.Executors
import java.nio.charset.StandardCharsets
import _root_.fp_in_scala_exercises.ch13.TailRec.Suspend
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileReader
import java.io.FileWriter
object ch13 {
  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): TailRec[Unit] =
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

  sealed trait TailRec[A] {
    def run: A = {
      @tailrec
      def run0[A, B](io: TailRec[A]): A =
        io match {
          case TailRec.Return(a)  => a
          case TailRec.Suspend(a) => a()
          case TailRec.FlatMap(sub, g) =>
            sub match {
              case TailRec.Return(a)  => run0(g(a))
              case TailRec.Suspend(a) => run0(g(a()))
              case TailRec.FlatMap(sub2, f) =>
                val transformed =
                  TailRec.FlatMap(sub2, (x: Any) => TailRec.FlatMap(f(x), g))
                run0(transformed)
            }
        }
      run0(this)
    }

    def skip: TailRec[Unit] = this >* TailRec.unit(())

    def **[B](io: TailRec[B]): TailRec[(A, B)] =
      TailRec.product(this, io)

    def >*[B](io: TailRec[B]): TailRec[B] =
      TailRec.map2(this, io)((_, b) => b)

    def map[B](f: A => B): TailRec[B] =
      flatMap(a => TailRec.unit(f(a)))

    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      TailRec.FlatMap(this, f)
  }

  object TailRec extends Monad[TailRec] {
    case class Return[A](a: A) extends TailRec[A]
    case class Suspend[A](a: () => A) extends TailRec[A]
    case class FlatMap[A, B](io: TailRec[A], f: A => TailRec[B])
        extends TailRec[B]

    def unit[A](f: => A): TailRec[A] = Return(f)
    override def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      a.flatMap(f)
  }

  def printLine(msg: String): TailRec[Unit] =
    TailRec.Suspend(() => println(msg))
  def readLine: TailRec[String] = TailRec.Suspend(() => scala.io.StdIn.readLine)

  def fahrenheitToCelsius(degF: Double): Double =
    (degF - 32) * 5.0 / 9.0

  def converter: TailRec[Unit] =
    for {
      _ <- printLine("Enter a temperature in degrees Fahrenheit")
      degC <- readLine.map(line => fahrenheitToCelsius(line.toDouble))
      _ <- printLine(degC.toString())
    } yield ()

  val echo: TailRec[Unit] = readLine.flatMap(printLine(_))
  val readInt: TailRec[Int] = readLine.map(_.toInt)
  val readInts: TailRec[List[Int]] =
    readLine.map(_.split(" ").map(_.toInt).toList)

  case class Ref[A](var a: A) {
    def modify(f: A => A): TailRec[Unit] = {
      a = f(a)
      TailRec.unit(())
    }

    def get: TailRec[A] = TailRec.unit(a)
  }

  def ref[A](a: A): TailRec[Ref[A]] = TailRec.unit(Ref(a))

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

  def factorial2(n: Int): TailRec[Int] =
    for {
      acc <- ref(1)
      _ <- TailRec.foreachM(1.to(n).toStream)(i => acc.modify(_ * i))
      fac <- acc.get
    } yield fac

  def PrintLines(msg: String*): TailRec[Unit] =
    msg.toList.foldLeft(TailRec.unit(())) {
      case (io, s) => io >* printLine(s)
    }

  val factorialRepl: TailRec[Unit] =
    PrintLines(
      "The Amazing Factorial REPL, v2.0",
      "q - quit",
      "<number> - compute the factorial of the given number",
      "<anything else> - crash spectacularly"
    ) >* TailRec.doWhile(readLine) { line =>
      TailRec.when(line != "q") {
        for {
          fac <- factorial2(line.toInt)
          _ <- printLine(fac.toString)
        } yield ()
      }
    }

  def composeMany(a: Int): Int =
    List
      .fill(10000000)((a: Int) => TailRec.unit(a + 1))
      .foldRight((a: Int) => TailRec.unit(a))((f, g) =>
        a => f(a).flatMap(g(_))
      )(a)
      .run

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      Async.FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(a => Async.unit(f(a)))

    def run: Par[A] = {
      @tailrec
      def flatten(a: Async[A]): Async[A] =
        a match {
          case Async.FlatMap(Async.Return(a), f) => flatten(f(a))
          case Async.FlatMap(Async.FlatMap(a, f), g) =>
            flatten(Async.FlatMap(a, (a: Any) => Async.FlatMap(f(a), g)))
          case a => a
        }
      flatten(this) match {
        case Async.Return(a)    => Par.unit(a)
        case Async.Suspend(par) => par
        case Async.FlatMap(Async.Suspend(par), f) =>
          Par.flatMap(par)(a => f(a).run)
        case _ => throw new Error("invariant violated")
      }
    }
  }

  object Async {
    case class Return[A](a: A) extends Async[A]
    case class Suspend[A](a: Par[A]) extends Async[A]
    case class FlatMap[A, B](a: Async[A], f: A => Async[B]) extends Async[B]
    def unit[A](a: A): Async[A] = Return(a)
  }

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      Free.FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(a => Free.unit(f(a)))
  }

  object Free { self =>
    case class Return[F[_], A](a: A) extends Free[F, A]
    case class Suspend[F[_], A](a: F[A]) extends Free[F, A]
    case class FlatMap[F[_], A, B](a: Free[F, A], f: A => Free[F, B])
        extends Free[F, B]
    def unit[F[_], A](a: A): Free[F, A] = Return(a)

    def monad[F[_]]: Monad[({ type T[A] = Free[F, A] })#T] =
      new Monad[({ type T[A] = Free[F, A] })#T] {
        override def flatMap[A, B](a: Free[F, A])(
            f: A => Free[F, B]
        ): Free[F, B] = a.flatMap(f(_))

        def unit[A](a: => A): Free[F, A] = self.unit(a)
      }

    @tailrec
    def runTrampoline[A](a: Free[Function0, A]): A =
      a match {
        case Return(a)  => a
        case Suspend(a) => a()
        case FlatMap(sub, f) =>
          sub match {
            case Return(a)  => runTrampoline(f(a))
            case Suspend(a) => runTrampoline(f(a()))
            case FlatMap(sub2, g) =>
              runTrampoline(sub2.flatMap(x => g(x).flatMap(f(_))))
          }
      }

    def run[F[_], G[_], A](
        a: Free[F, A]
    )(implicit Tr: F ~> G, G: Monad[G]): G[A] = {
      @tailrec
      def flatten(a: Free[F, A]): Free[F, A] =
        a match {
          case FlatMap(Return(a), f) => flatten(f(a))
          case FlatMap(FlatMap(sub, f), g) =>
            flatten(sub.flatMap(x => f(x).flatMap(g)))
          case a => a
        }

      flatten(a) match {
        case Return(a)              => G.unit(a)
        case Suspend(a)             => Tr(a)
        case FlatMap(Suspend(a), f) => G.flatMap(Tr(a))(a => run(f(a)))
        case _                      => throw new Error("invariant violated")
      }
    }

    def translate[F[_], G[_], A](a: Free[F, A])(Tr: F ~> G): Free[G, A] = {
      type FreeG[A] = Free[G, A]
      object TrFree extends (F ~> FreeG) {
        def apply[A](a: F[A]): FreeG[A] = Suspend(Tr(a))
      }
      run(a)(TrFree, monad[G])
    }
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      Par.fork { Par.flatMap(a)(f) }
  }

  val function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): Function0[A] = () => a

    override def flatMap[A, B](a: Function0[A])(
        f: A => Function0[B]
    ): Function0[B] = () => f(a())()
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLine: ConsoleIO[Option[String]] = Free.Suspend(ReadLine)

    def printLine(line: String): ConsoleIO[Unit] =
      Free.Suspend(PrintLine(line))

    val toFunction0: Console ~> Function0 =
      new (Console ~> Function0) {
        def apply[A](a: Console[A]): () => A = a.toThunk
      }

    def runFunction0[A](a: ConsoleIO[A]): () => A =
      Free.run(a)(toFunction0, function0Monad)

    val toPar: Console ~> Par =
      new (Console ~> Par) {
        def apply[A](a: Console[A]): Par[A] = a.toPar
      }

    def runPar[A](a: ConsoleIO[A]): Par[A] =
      Free.run(a)(toPar, parMonad)

    def run[A](a: ConsoleIO[A]): A =
      Free.runTrampoline(Free.translate(a)(toFunction0))

    val toReader: Console ~> ConsoleReader =
      new (Console ~> ConsoleReader) {
        def apply[A](a: Console[A]): ConsoleReader[A] =
          a match {
            case ReadLine        => ConsoleReader(s => Some(s))
            case PrintLine(line) => ConsoleReader.unit(())
          }
      }

    def runReader[A](a: ConsoleIO[A]): ConsoleReader[A] =
      Free.run(a)(toReader, ConsoleReader)

    val toState: Console ~> ConsoleState =
      new (Console ~> ConsoleState) {
        def apply[A](a: Console[A]): ConsoleState[A] =
          a match {
            case ReadLine        => ConsoleState.readLine
            case PrintLine(line) => ConsoleState.printLine(line)
          }
      }

    def runState[A](a: ConsoleIO[A]): ConsoleState[A] =
      Free.run(a)(toState, ConsoleState)

    val toTailReader: Console ~> TailConsoleReader =
      new (Console ~> TailConsoleReader) {
        def apply[A](a: Console[A]): TailConsoleReader[A] =
          a match {
            case ReadLine        => TailConsoleReader(s => TailRec.unit(Some(s)))
            case PrintLine(line) => TailConsoleReader(s => TailRec.unit(()))
          }
      }

    def runTailReader[A](a: ConsoleIO[A]): TailConsoleReader[A] =
      Free.run(a)(toTailReader, TailConsoleReader)

    val toTailState: Console ~> TailConsoleState =
      new (Console ~> TailConsoleState) {
        def apply[A](a: Console[A]): TailConsoleState[A] =
          a match {
            case ReadLine        => TailConsoleState.readLine
            case PrintLine(line) => TailConsoleState.printLine(line)
          }
      }

    def runTailState[A](a: ConsoleIO[A]): TailConsoleState[A] =
      Free.run(a)(toTailState, TailConsoleState)
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar: Par[Option[String]] = Par.lazyUnit(readLine)
    def toThunk: Function0[Option[String]] = () => readLine

    def readLine: Option[String] =
      try {
        Some(scala.io.StdIn.readLine)
      } catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar: Par[Unit] = Par.lazyUnit(println(line))
    def toThunk: Function0[Unit] = () => println(line)
  }

  trait Translate[F[_], G[_]] {
    def apply[A](a: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  val f1: Free[Console, Option[String]] = for {
    _ <- Console.printLine("the quick brown fox jumps over the lazy dogs")
    line <- Console.readLine
  } yield line

  case class ConsoleReader[A](run: String => A) {
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(s => f(run(s)).run(s))

    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(s => f(run(s)))
  }

  object ConsoleReader extends Monad[ConsoleReader] {
    def unit[A](a: => A): ConsoleReader[A] =
      ConsoleReader(_ => a)

    override def flatMap[A, B](a: ConsoleReader[A])(
        f: A => ConsoleReader[B]
    ): ConsoleReader[B] = a.flatMap(f(_))
  }

  case class Buffers(in: List[String], out: List[String])

  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState { buffers =>
        run(buffers) match {
          case (a, buffers) => f(a).run(buffers)
        }
      }
  }

  object ConsoleState extends Monad[ConsoleState] {
    def unit[A](a: => A): ConsoleState[A] = ConsoleState((a, _))

    override def flatMap[A, B](a: ConsoleState[A])(
        f: A => ConsoleState[B]
    ): ConsoleState[B] = a.flatMap(f(_))

    def readLine: ConsoleState[Option[String]] =
      ConsoleState {
        case Buffers(in, out) =>
          (in.headOption, Buffers(in.drop(1), out))
      }

    def printLine(line: String): ConsoleState[Unit] =
      ConsoleState {
        case Buffers(in, out) =>
          ((), Buffers(in, line :: out))
      }
  }

  case class TailConsoleReader[A](run: String => TailRec[A]) {
    def flatMap[B](f: A => TailConsoleReader[B]): TailConsoleReader[B] =
      TailConsoleReader { s =>
        run(s).flatMap(f(_).run(s))
      }

    def map[B](f: A => B): TailConsoleReader[B] =
      TailConsoleReader { s =>
        run(s).map(f(_))
      }
  }

  object TailConsoleReader extends Monad[TailConsoleReader] {
    def unit[A](a: => A): TailConsoleReader[A] =
      TailConsoleReader(_ => TailRec.unit(a))

    override def flatMap[A, B](a: TailConsoleReader[A])(
        f: A => TailConsoleReader[B]
    ): TailConsoleReader[B] = a.flatMap(f(_))
  }

  case class TailConsoleState[A](run: Buffers => TailRec[(A, Buffers)]) {
    def flatMap[B](f: A => TailConsoleState[B]): TailConsoleState[B] =
      TailConsoleState { buffers =>
        run(buffers).flatMap {
          case (a, buffers) => f(a).run(buffers)
        }
      }
  }

  object TailConsoleState extends Monad[TailConsoleState] {
    def unit[A](a: => A): TailConsoleState[A] =
      TailConsoleState { buffers =>
        TailRec.Return(a, buffers)
      }

    override def flatMap[A, B](a: TailConsoleState[A])(
        f: A => TailConsoleState[B]
    ): TailConsoleState[B] = a.flatMap(f(_))

    def readLine: TailConsoleState[Option[String]] =
      TailConsoleState {
        case Buffers(in, out) =>
          TailRec.Return((in.headOption, Buffers(in.drop(1), out)))
      }

    def printLine(line: String): TailConsoleState[Unit] =
      TailConsoleState {
        case Buffers(in, out) =>
          TailRec.Return(((), Buffers(in, line :: out)))
      }
  }

  val p: Console.ConsoleIO[Unit] =
    for {
      _ <- Console.printLine("What's your name?")
      name <- Console.readLine
      _ <- name match {
        case Some(name) => Console.printLine(s"Hello, $name!")
        case None       => Console.printLine(s"Fine, be that way.")
      }
    } yield ()

  trait Source {
    def readBytes(
        numBytes: Int,
        callback: Either[Throwable, Array[Byte]] => Unit
    ): Unit

    def nonblockingRead(numBytes: Int): Par[Array[Byte]] =
      Par.async(readBytes(numBytes, _))

    def readPar(source: Source, numBytes: Int): Free[Par, Array[Byte]] =
      Free.Suspend(nonblockingRead(numBytes))
  }

  object aio {
    implicit def stringToPath(path: String): Path =
      FileSystems.getDefault().getPath(path)

    val read: OpenOption = StandardOpenOption.READ
    val write: OpenOption = StandardOpenOption.WRITE
    val create: OpenOption = StandardOpenOption.CREATE

    def open(path: Path, opts: OpenOption*): Par[AsynchronousFileChannel] =
      Par.lazyUnit {
        AsynchronousFileChannel.open(path, opts: _*)
      }

    def read(
        file: AsynchronousFileChannel,
        position: Long,
        numBytes: Int
    ): Par[Array[Byte]] = {
      val buffer = ByteBuffer.allocate(numBytes)
      Par.async { f =>
        val handler = new CompletionHandler[Integer, Unit] {
          def completed(bytesRead: Integer, x: Unit): Unit =
            f(Right(buffer.array().take(bytesRead)))

          def failed(error: Throwable, x: Unit): Unit =
            f(Left(error))
        }
        file.read(buffer, position, (), handler)
      }
    }

    def write(
        file: AsynchronousFileChannel,
        position: Long,
        bytes: Array[Byte]
    ): Par[Int] =
      Par.async { f =>
        val buffer = ByteBuffer.wrap(bytes)

        val handler = new CompletionHandler[Integer, Unit] {
          def completed(bytesWritten: Integer, x: Unit): Unit =
            f(Right(bytesWritten))

          def failed(error: Throwable, x: Unit): Unit =
            f(Left(error))
        }

        file.write(buffer, position, (), handler)
      }

    def close(file: AsynchronousFileChannel): Par[Unit] =
      Par.lazyUnit {
        file.close()
      }

    def test: Unit = {
      val res = for {
        file <- open("scratch.txt", read, write, create)
        w <- write(
          file,
          0,
          "the quick brown fox jumps over the lazy dogs".getBytes
        )
        w2 <- write(file, 16, "cat".getBytes)
        w3 <- write(file, 40, "oxen".getBytes)
        s <- read(file, 0, 1000)
        s2 <- read(file, 16, 3)
        s3 <- read(file, 40, 4)
        _ <- close(file)
      } yield {
        val ok =
          w == 44 &&
            w2 == 3 &&
            w3 == 4 &&
            s.sameElements(
              "the quick brown cat jumps over the lazy oxen".getBytes
            ) &&
            s2.sameElements("cat".getBytes) &&
            s3.sameElements("oxen".getBytes)
        if (ok) {
          Right(())
        } else {
          Left(
            (
              new String(s, StandardCharsets.UTF_8),
              new String(s2, StandardCharsets.UTF_8),
              new String(s3, StandardCharsets.UTF_8)
            )
          )
        }
      }

      val ex = Executors.newCachedThreadPool
      println(Par.run(ex)(res))
      ex.shutdown()
    }
  }

  type IO[A] = Free[Par, A]

  abstract class App {
    def performIO[A](a: IO[A])(ex: ExecutorService): A = {
      val tr = new (Par ~> Par) { def apply[A](a: Par[A]): Par[A] = a }
      Par.run(ex)(Free.run(a)(tr, parMonad))
    }

    def main(args: Array[String]): Unit =
      performIO(pureMain(args))(Executors.newFixedThreadPool(8))

    def pureMain(args: IndexedSeq[String]): IO[Unit]
  }

  sealed trait Files[A] {
    def run: A
  }

  type HandleR = BufferedReader
  type HandleW = BufferedWriter

  case class OpenRead(path: String) extends Files[HandleR] {
    def run: HandleR = new BufferedReader(new FileReader(path))
  }

  case class OpenWrite(path: String) extends Files[HandleW] {
    def run: HandleW = new BufferedWriter(new FileWriter(path))
  }

  case class ReadLine(file: HandleR) extends Files[Option[String]] {
    def run: Option[String] = Option(file.readLine)
  }

  case class WriteLine(file: HandleW, line: String) extends Files[Unit] {
    def run: Unit = file.write(line + "\n")
  }

  case class CloseRead(file: HandleR) extends Files[Unit] {
    def run: Unit = file.close()
  }

  case class CloseWrite(file: HandleW) extends Files[Unit] {
    def run: Unit = file.close()
  }

  object Files {
    val toFunction0: Files ~> Function0 = new (Files ~> Function0) {
      def apply[A](a: Files[A]): () => A = () => a.run
    }
  }

  val p2: Free[Files, Unit] = {
    def loop(r: HandleR, w: HandleW): Free[Files, Unit] =
      for {
        lineOpt <- Free.Suspend(ReadLine(r))
        _ <- lineOpt match {
          case Some(line) =>
            val celsius = fahrenheitToCelsius(line.toDouble)
            Free
              .Suspend(WriteLine(w, celsius.toString))
              .flatMap(_ => loop(r, w))
          case None =>
            Free.Return[Files, Unit](())
        }
      } yield ()

    for {
      r <- Free.Suspend(OpenRead("fahrenheit.txt"))
      w <- Free.Suspend(OpenWrite("celsius.txt"))
      _ <- loop(r, w)
      _ <- Free.Suspend(CloseRead(r))
      _ <- Free.Suspend(CloseWrite(w))
    } yield ()
  }

  def testFiles: Unit = Free.run(p2)(Files.toFunction0, function0Monad)()
}
