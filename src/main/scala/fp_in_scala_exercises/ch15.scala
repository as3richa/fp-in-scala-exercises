package fp_in_scala_exercises
import ch12.Monad
import ch13.{IO, Free, TailRec}
import ch13.TailRec.{Return, Suspend}
import ch5.{Stream, Empty, Cons}
import ch7.nonblocking.Par
import ch8.{Gen, forAll, run, Prop}
import java.io.{BufferedReader, FileReader}
import java.io.BufferedWriter
import java.io.FileWriter
import java.util.concurrent.{Executors, ExecutorService}
import scala.annotation.tailrec
import scala.collection.BufferedIterator

object ch15 {
  def linesGt40k(filename: String): IO[Boolean] =
    for {
      file <-
        Free.Suspend(Par.unit(new BufferedReader(new FileReader(filename))))

      lines <- {
        def countLines(max: Int): IO[Int] =
          Free.Suspend { Par.unit(file.readLine()) }.flatMap { a =>
            if (a == null || max == 0) {
              Free.Return(0)
            } else {
              countLines(max - 1).map(_ + 1)
            }
          }
        countLines(40000)
      }

      _ <- Free.Suspend(Par.unit(file.close()))
    } yield lines >= 40000

  sealed trait Process[-I, +O] {
    import Process.emit

    def run(in: Stream[I]): Stream[O] = {
      @tailrec
      def run0(p: Process[I, O], in: Stream[I]): Stream[O] =
        p match {
          case Emit(head, tail) => Stream.cons(head, tail().run(in))
          case Await(recv) =>
            in match {
              case Cons(head, tail) => run0(recv(Some(head())), tail())
              case Empty            => run0(recv(None), Empty)
            }
          case Halt() => Empty
        }
      run0(this, in)
    }

    def processFile[A](file: java.io.File, z: A)(
        g: (A, O) => A
    )(implicit ev: String <:< I): IO[A] =
      IO.par {
        @tailrec
        def processLines(
            p: Process[I, O],
            lines: Iterator[String],
            a: IO[A]
        ): IO[A] =
          p match {
            case Emit(head, tail) =>
              processLines(tail(), lines, a.map(g(_, head)))
            case Await(recv) =>
              if (lines.hasNext) {
                processLines(recv(Some(lines.next())), lines, a)
              } else {
                processLines(recv(None), lines, a)
              }
            case Halt() => a
          }

        val source = io.Source.fromFile(file)

        try {
          Free.runPar(
            processLines(this, source.getLines().buffered, Free.Return(z))
          )
        } finally {
          source.close()
        }
      }

    def transduceFile(
        outputFile: java.io.File,
        inputFile: java.io.File
    )(implicit evi: String <:< I, evo: O <:< String): IO[Unit] = {
      val writer = new BufferedWriter(new FileWriter(outputFile))
      try {
        processFile(inputFile, ()) { (_, line) =>
          writer.write(line + "\n")
        }

      } finally {
        writer.close()
      }
    }

    def chain[Q](after: Process[O, Q]): Process[I, Q] =
      after match {
        case Emit(head, tail) => emit(head, chain(tail()))
        case Await(recv) =>
          this match {
            case Emit(head, tail) => tail().chain(recv(Some(head)))
            case Await(recv0)     => Await(recv0(_).chain(after))
            case Halt()           => Halt().chain(recv(None))
          }
        case Halt() => Halt()
      }

    def |>[Q](after: Process[O, Q]): Process[I, Q] = chain(after)

    def append[I2 <: I, O2 >: O](after: Process[I2, O2]): Process[I2, O2] =
      this match {
        case Emit(head, tail) => emit(head, tail().append(after))
        case Await(recv)      => Await(recv(_).append(after))
        case Halt()           => after
      }

    def ++[I2 <: I, O2 >: O](after: Process[I2, O2]): Process[I2, O2] =
      append(after)

    def flatMap[I2 <: I, O2](f: O => Process[I2, O2]): Process[I2, O2] =
      this match {
        case Emit(head, tail) => f(head) ++ tail().flatMap(f(_))
        case Await(recv)      => Await { recv(_).flatMap(f(_)) }
        case Halt()           => Halt()
      }

    def zip[I2 <: I, O2](other: Process[I2, O2]): Process[I2, (O, O2)] = {
      def feed[O](p: Process[I2, O], in: Option[I2]): Process[I2, O] =
        p match {
          case Emit(head, tail) => emit(head, feed(tail(), in))
          case Await(recv)      => recv(in)
          case Halt()           => Halt()
        }

      (this, other) match {
        case (Await(recv), _) =>
          Await(in => recv(in).zip(feed(other, in)))
        case (_, Await(recv)) =>
          Await(in => feed(this, in).zip(recv(in)))
        case (Emit(head1, tail1), Emit(head2, tail2)) =>
          emit((head1, head2), tail1().zip(tail2()))
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
      }
    }

    def take(n: Int): Process[I, O] =
      chain(Process.take(n))

    def takeWhile(f: O => Boolean) =
      chain(Process.takeWhile(f))

    def drop(n: Int): Process[I, O] =
      chain(Process.drop(n))

    def dropWhile(f: O => Boolean): Process[I, O] =
      chain(Process.dropWhile(f))

    def filter(f: O => Boolean): Process[I, O] =
      chain(Process.filter(f))

    def zipWithIndex: Process[I, (O, Int)] =
      chain(Process.zipWithIndex)

    def repeat: Process[I, O] = {
      def repeat0(pr: Process[I, O]): Process[I, O] =
        pr match {
          case Emit(head, tail) =>
            emit(head, repeat0(tail()))
          case Await(recv) =>
            Await {
              case Some(in) => repeat0(recv(Some(in)))
              case None     => recv(None)
            }
          case Halt() => repeat0(this)
        }

      repeat0(this)
    }

    def map[O2](f: O => O2): Process[I, O2] =
      chain(Process.lift(f))

    def exists(f: O => Boolean): Process[I, Boolean] =
      chain(Process.exists(f))
  }

  object Process {
    def monad[I]: Monad[({ type T[A] = Process[I, A] })#T] =
      new Monad[({ type T[A] = Process[I, A] })#T] {
        def unit[A](a: => A): Process[I, A] = Emit(a)

        override def flatMap[A, B](a: Process[I, A])(
            f: A => Process[I, B]
        ): Process[I, B] = a.flatMap(f(_))
      }

    def emit[I, O](head: O, tail: => Process[I, O] = Halt()): Process[I, O] =
      Emit(head, () => tail)

    def apply[O](xs: O*): Process[Any, O] =
      xs.foldRight[Process[Any, O]](Halt()) { emit(_, _) }

    def apply[O](xs: Stream[O]): Process[Any, O] =
      xs.foldRight[Process[Any, O]](Halt())(emit(_, _))

    def constant[O](x: O): Process[Any, O] =
      emit(x, constant(x))

    def liftOne[A, B](f: A => B): Process[A, B] =
      await { a =>
        emit(f(a))
      }

    def lift[A, B](f: A => B): Process[A, B] =
      await { a =>
        emit(f(a), lift(f))
      }

    def lift2[A, B](f: A => B): Process[A, B] =
      liftOne(f).repeat

    def take[O](n: Int): Process[O, O] =
      if (n <= 0) {
        Halt()
      } else {
        await { (out: O) =>
          emit(out, take(n - 1))
        }
      }

    def takeWhile[O](f: O => Boolean): Process[O, O] =
      Await[O, O] {
        case Some(out) if f(out) => emit(out, takeWhile(f))
        case _                   => Halt()
      }

    def echo[O]: Process[O, O] =
      await { (out: O) =>
        emit(out, echo)
      }

    def drop[O](n: Int): Process[O, O] =
      if (n <= 0) {
        echo
      } else {
        Await(_ => drop(n - 1))
      }

    def dropWhile[O](f: O => Boolean): Process[O, O] =
      Await {
        case Some(out) =>
          if (f(out)) {
            dropWhile(f)
          } else {
            emit(out, echo)
          }
        case _ => echo
      }

    def filter[O](f: O => Boolean): Process[O, O] =
      await { (out: O) =>
        if (f(out)) {
          emit(out, filter(f(_)))
        } else {
          filter(f)
        }
      }

    def filter2[O](f: O => Boolean): Process[O, O] =
      await { (out: O) =>
        if (f(out)) {
          Emit(out)
        } else {
          Halt()
        }
      }.repeat

    def zipWithIndex[O]: Process[O, (O, Int)] = {
      def zipWithIndex0(start: Int): Process[O, (O, Int)] =
        await { out =>
          emit((out, start), zipWithIndex0(start + 1))
        }
      zipWithIndex0(0)
    }

    def duplicate[O](n: Int): Process[O, O] = {
      def duplicateElem(k: Int, out: O): Process[O, O] =
        if (k <= 0) {
          duplicate(n)
        } else {
          emit(out, duplicateElem(k - 1, out))
        }

      await { out => duplicateElem(n, out) }
    }

    def duplicate2[O](n: Int): Process[O, O] =
      if (n <= 0) {
        Halt()
      } else {
        await { (out: O) =>
          0.until(n).foldLeft[Process[O, O]](Halt())((p, _) => emit(out, p))
        }.repeat
      }

    def count: Process[Any, Int] = {
      def count0(count: Int): Process[Any, Int] =
        await { _ =>
          emit(count, count0(count + 1))
        }

      count0(0)
    }

    def count2: Process[Any, Int] =
      loop(0)((in, count) => (count, count + 1))

    def sum: Process[Double, Double] = {
      def sum0(s: Double): Process[Double, Double] =
        await { value =>
          emit(s + value, sum0(s + value))
        }

      sum0(0)
    }

    def sum2: Process[Double, Double] =
      loop(0.0)((in, sum) => (sum + in, sum + in))

    def mean: Process[Double, Double] = {
      def mean0(s: Double, count: Int): Process[Double, Double] =
        await { value =>
          emit((s + value) / (count + 1), mean0(s + value, count + 1))
        }

      mean0(0, 0)
    }

    def mean2: Process[Double, Double] =
      loop((0.0, 0)) {
        case (in, (sum, count)) =>
          ((sum + in) / (count + 1), (sum + in, count + 1))
      }

    def mean3: Process[Double, Double] =
      sum.zip(count).map {
        case (sum, count) => sum / (count + 1)
      }

    def await[I, O](f: I => Process[I, O]): Process[I, O] =
      Await {
        case Some(in) => f(in)
        case None     => Halt()
      }

    def loop[S, I, O](s: S)(f: (I, S) => (O, S)): Process[I, O] = {
      def loop0(s: S): Process[I, O] =
        await { in =>
          val (out, s2) = f(in, s)
          emit(out, loop0(s2))
        }

      loop0(s)
    }

    def exists[I](f: I => Boolean): Process[I, Boolean] =
      Await {
        case Some(in) =>
          if (f(in)) {
            emit(true)
          } else {
            exists(f)
          }
        case None => emit(false)
      }
  }

  case class Emit[-I, +O](head: O, tail: () => Process[I, O] = () => Halt())
      extends Process[I, O]

  case class Await[-I, +O](recv: Option[I] => Process[I, O])
      extends Process[I, O]

  case class Halt() extends Process[Any, Nothing]

  def testProcess: Unit = {
    val g = Gen.int.listOfN(Gen.choose(0, 100000))
    val gl = g.flatMap(list => Gen.choose(0, list.length + 1).map((list, _)))

    val props = Seq(
      forAll(g) { list =>
        Process
          .liftOne((x: Int) => x + 1337)
          .run(Stream(list: _*))
          .toList == list.headOption.map(_ + 1337).toList
      },
      forAll(g) { list =>
        Process.lift((x: Int) => x + 1337).run(Stream(list: _*)).toList == list
          .map(_ + 1337)
      },
      forAll(g) { list =>
        Process.lift2((x: Int) => x + 1337).run(Stream(list: _*)).toList == list
          .map(_ + 1337)
      },
      forAll(gl) {
        case (list, i) =>
          Process.take(i).run(Stream(list: _*)).toList == list.take(i)
      },
      forAll(gl) {
        case (list, i) =>
          Process.drop(i).run(Stream(list: _*)).toList == list.drop(i)
      },
      forAll(g) { list =>
        Process
          .filter((x: Int) => x % 2 == 0)
          .run(Stream(list: _*))
          .toList == list
          .filter(_ % 2 == 0)
      },
      forAll(g) { list =>
        Process
          .takeWhile((x: Int) => x % 2 == 0)
          .run(Stream(list: _*))
          .toList == list
          .takeWhile(_ % 2 == 0)
      },
      forAll(g) { list =>
        Process
          .dropWhile((x: Int) => x % 2 == 0)
          .run(Stream(list: _*))
          .toList == list
          .dropWhile(_ % 2 == 0)
      },
      forAll(g) { list =>
        Process.zipWithIndex
          .run(Stream(list: _*))
          .toList == list.zipWithIndex
      },
      forAll(g) { list =>
        Process
          .duplicate(5)
          .run(Stream(list: _*))
          .toList == list.flatMap(List.fill(5)(_))
      },
      forAll(g) { list =>
        Process
          .duplicate2(5)
          .run(Stream(list: _*))
          .toList == list.flatMap(List.fill(5)(_))
      },
      forAll(g) { list =>
        Process.count
          .run(Stream(list: _*))
          .toList == 0.until(list.length).toList
      },
      forAll(g) { list =>
        Process.count2
          .run(Stream(list: _*))
          .toList == 0.until(list.length).toList
      },
      Prop.check {
        Process.count
          .take(1000)
          .map(_ * 5)
          .run(Stream.constant(()))
          .toList == 0.until(1000).map(_ * 5)
      },
      forAll(g.map(_.map(_.toDouble))) { list =>
        Process.sum
          .run(Stream(list: _*))
          .toList == 1.to(list.length).map(list.take(_).sum).toList
      },
      forAll(g.map(_.map(_.toDouble))) { list =>
        Seq(
          Process.mean,
          Process.mean2,
          Process.mean3
        ).forall(
          _.run(Stream(list: _*)).toList == 1
            .to(list.length)
            .map(count => list.take(count).sum / count)
            .toList
        )
      },
      forAll(gl) {
        case (list, i) =>
          (Process.echo[Int].take(i) ++ Process.count)
            .run(Stream(list: _*))
            .toList == list.take(i) ++ 0.until(list.length - i)
      },
      forAll(g ** g) {
        case (list1, list2) =>
          Process(list1: _*).run(Stream(list2: _*)).toList == list1
      },
      Prop.check {
        Process(0.until(1000000): _*).run(Stream()).toList == 0
          .until(1000000)
          .toList
      },
      forAll(g ** g) {
        case (list1, list2) =>
          Process(Stream(list1: _*)).run(Stream(list2: _*)).toList == list1
      },
      forAll(g) { input =>
        Process(Stream.constant(1))
          .take(1000)
          .run(Stream(input: _*))
          .toList == List.fill(1000)(1)
      },
      forAll(g) { input =>
        input.forall(x =>
          Process
            .exists((y: Int) => y == x)
            .run(Stream(input: _*))
            .toList == List(true)
        ) && Process
          .exists((y: Int) => false)
          .run(Stream(input: _*))
          .toList == List(false)
      },
      forAll(g ** g) {
        case (list1, list2) =>
          Process(list1: _*)
            .zip(Process.count)
            .run(Stream(list2: _*))
            .toList == list1.take(list2.length).zipWithIndex
      }
    )

    props.foreach(run(_))
  }

  def linesGt40k2(filename: String): IO[Boolean] = {
    val input = new java.io.File(filename)
    Process.count
      .exists(_ >= 40000)
      .processFile(input, Option.empty[Boolean])((_, exists) => Some(exists))
      .map(_.get)
  }

  def runCelsiusToFahrenheit(outputName: String, inputName: String): Unit = {
    val outputFile = new java.io.File(outputName)
    val inputFile = new java.io.File(inputName)

    val io = Process
      .lift[String, String] { line =>
        val fahrenheit = line.toDouble
        val celsius = (fahrenheit - 32) * 5 / 9
        celsius.toString
      }
      .transduceFile(outputFile, inputFile)

    Par.run(Executors.newCachedThreadPool)(Free.runPar(io))
  }

  object Extensible {
    sealed trait Process[F[_], O] {
      import Process.{tryHalt, await}

      def onHalt[O2 >: O](
          f: Throwable => Process[F, O2]
      ): Process[F, O2] =
        this match {
          case Emit(head, tail) =>
            Emit(head, tail.map(_.onHalt(f)))
          case Await(req, recv) =>
            await(req)(err =>
              Suspend(() => recv(err)).flatMap(_.map(_.onHalt(f)))
            )
          case Halt(err) =>
            tryHalt(f(err))
        }

      def ++[O2 >: O](after: Process[F, O2]): Process[F, O2] =
        onHalt {
          case End => after
          case err => Halt(err)
        }

      def map[O2](f: O => O2): Process[F, O2] =
        this match {
          case Emit(head, tail) =>
            tryHalt(Emit(f(head), tail.map(_.map(f(_)))))
          case Await(req, recv) => await(req)(recv(_).map(_.map(f(_))))
          case Halt(err)        => Halt(err)
        }

      def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = {
        def flatMap0(p: Process[F, O]): TailRec[Process[F, O2]] =
          p match {
            case Emit(head, tail) =>
              tail.map { tail =>
                tryHalt(f(head)) ++ (tail.flatMap(f(_)))
              }
            case Await(req, recv) =>
              Return(await(req)(recv(_).map(_.flatMap(f(_)))))
            case Halt(err) => Return(Halt(err))
          }
        flatMap0(this).run
      }

      def repeat: Process[F, O] = {
        def repeat0(p: Process[F, O]): TailRec[Process[F, O]] =
          p match {
            case Emit(head, tail) =>
              tail.map { tail =>
                Emit(head, repeat0(tail))
              }
            case Await(req, recv) =>
              Return(await(req) {
                case Right(value) => recv(Right(value)).flatMap(repeat0(_))
                case Left(err)    => recv(Left(err))
              })
            case Halt(End) => Return(this)
            case Halt(err) => Return(Halt(err))
          }
        repeat0(this).run
      }

      def filter(f: O => Boolean): Process[F, O] = {
        def filter0(p: Process[F, O]): TailRec[Process[F, O]] =
          p match {
            case Emit(head, tail) =>
              val rest = tail.flatMap(filter0(_))
              try {
                if (f(head)) {
                  Return(Emit(head, rest))
                } else {
                  rest
                }
              } catch {
                case err: Throwable => Return(Halt(err))
              }
            case Await(req, recv) =>
              Return(await(req)(recv(_).flatMap(filter0(_))))
            case Halt(err) => Return(Halt(err))
          }
        filter0(this).run
      }

      def take(n: Int): Process[F, O] =
        if (n <= 0) {
          Halt(End)
        } else {
          this match {
            case Emit(head, tail) =>
              Emit(head, tail.flatMap(tail => Suspend(() => tail.take(n - 1))))
            case Await(req, recv) => await(req)(recv(_).map(_.take(n - 1)))
            case Halt(err)        => Halt(err)
          }
        }

      def takeWhile(f: O => Boolean): Process[F, O] =
        this match {
          case Emit(head, tail) =>
            tryHalt {
              if (f(head)) {
                Emit(
                  head,
                  tail.flatMap(tail => Suspend(() => tail.takeWhile(f(_))))
                )
              } else {
                Halt(End)
              }
            }
          case Await(req, recv) =>
            await(req) { x =>
              Suspend(() => recv(x)).flatMap(_.map(_.takeWhile(f)))
            }
          case Halt(err) => Halt(err)
        }

      def drop(n: Int): Process[F, O] = {
        def drop0(p: Process[F, O], n: Int): TailRec[Process[F, O]] =
          if (n <= 0) {
            Return(p)
          } else {
            p match {
              case Emit(head, tail) => tail.flatMap(drop0(_, n - 1))
              case Await(req, recv) =>
                Return(await(req)(recv(_).flatMap(drop0(_, n - 1))))
              case Halt(err) => Return(Halt(err))
            }
          }
        drop0(this, n).run
      }

      def dropWhile(f: O => Boolean): Process[F, O] = {
        def dropWhile0(p: Process[F, O]): TailRec[Process[F, O]] =
          p match {
            case Emit(head, tail) =>
              try {
                if (f(head)) {
                  tail.flatMap(dropWhile0(_))
                } else {
                  Return(Halt(End))
                }
              } catch {
                case err: Throwable => Return(Halt(err))
              }
            case Await(req, recv) =>
              Return(await(req) { x =>
                Suspend(() => recv(x)).flatMap(_.flatMap(dropWhile0(_)))
              })
            case Halt(err) => Return(Halt(err))
          }
        dropWhile0(this).run
      }

      def zip[O2](other: Process[F, O2]): Process[F, (O, O2)] = {
        def feed[O](
            p: Process[F, O],
            x: Either[Throwable, Any]
        ): TailRec[Process[F, O]] =
          p match {
            case Emit(head, tail) =>
              Return(Emit(head, tail.flatMap(feed(_, x))))
            case Await(req, recv) => recv(x)
            case Halt(err)        => Return(Halt(err))
          }

        def zip0(
            p: Process[F, O],
            q: Process[F, O2]
        ): Process[F, (O, O2)] =
          (p, q) match {
            case (Emit(head, tail), Emit(qHead, qTail)) =>
              Emit((head, qHead), TailRec.map2(tail, qTail)(_.zip(_)))
            case (Await(req, recv), _) =>
              await(req)(x => TailRec.map2(recv(x), feed(q, x))(zip0(_, _)))
            case (_, Await(req, recv)) =>
              await(req)(x => TailRec.map2(feed(p, x), recv(x))(zip0(_, _)))
            case (Halt(err), _) => Halt(err)
            case (_, Halt(err)) => Halt(err)
          }

        zip0(this, other)
      }

      def zipWithIndex: Process[F, (O, Int)] = {
        def zipWithIndex0(p: Process[F, O], n: Int): Process[F, (O, Int)] =
          p match {
            case Emit(head, tail) =>
              Emit(
                (head, n),
                tail.flatMap(tail => Suspend(() => zipWithIndex0(tail, n + 1)))
              )
            case Await(req, recv) =>
              await(req) { x =>
                Suspend(() => recv(x)).flatMap {
                  _.flatMap(q => Suspend(() => zipWithIndex0(q, n)))
                }
              }
            case Halt(err) => Halt(err)
          }
        zipWithIndex0(this, 0)
      }
    }

    object Process {
      def apply[F[_], O](xs: O*): Process[F, O] =
        if (xs.isEmpty) {
          Halt(End)
        } else {
          Emit(xs.head, Suspend(() => apply(xs.tail: _*)))
        }

      def constant[F[_], O](x: O): Process[F, O] =
        Emit(x, Suspend(() => constant(x)))

      def tryHalt[F[_], O](f: => Process[F, O]): Process[F, O] =
        try {
          f
        } catch {
          case err: Throwable => Halt(err)
        }

      def await[F[_], O, A](req: F[A])(
          recv: Either[Throwable, A] => TailRec[Process[F, O]]
      ): Process[F, O] = Await(req, recv)

      def runLog[O](
          ex: ExecutorService
      )(src: Process[IO, O]): IndexedSeq[O] = {
        @tailrec
        def runLog0(
            src: Process[IO, O],
            log: IndexedSeq[O]
        ): IndexedSeq[O] =
          src match {
            case Emit(head, tail) => runLog0(tail.run, log :+ head)
            case Await(req, recv) =>
              val msg =
                try {
                  Right(Par.run(ex)(Free.runPar(req)))
                } catch {
                  case err: Throwable => Left(err)
                }
              runLog0(recv(msg).run, log)
            case Halt(End) => log
            case Halt(err) => throw err
          }

        runLog0(src, IndexedSeq())
      }
    }

    case class Emit[F[_], O](head: O, tail: TailRec[Process[F, O]])
        extends Process[F, O]

    case class Await[F[_], O, A](
        req: F[A],
        recv: Either[Throwable, A] => TailRec[Process[F, O]]
    ) extends Process[F, O]

    case class Halt[F[_], O](err: Throwable) extends Process[F, O]

    case object End extends Exception
    case object Kill extends Exception

    import Process.await

    def readLines(filename: String): Process[IO, String] =
      await(
        IO(new BufferedReader(new FileReader(filename)))
      ) {
        case Right(reader) =>
          lazy val read: Process[IO, String] =
            await(IO(Option(reader.readLine()))) {
              case Right(Some(line)) =>
                Return(Emit(line, Return(read)))
              case Right(None) => Return(Halt(End))
              case Left(err)   => Return(Halt(err))
            }

          Return(read.onHalt { err =>
            await(IO(reader.close())) { _ =>
              Return(Halt(err))
            }
          })

        case Left(err) => Return(Halt(err))
      }

    def testProcess: Unit = {
      val g = Gen.int.listOfN(Gen.choose(0, 10000))
      val gl = g.flatMap(list => Gen.choose(0, list.length + 1).map((list, _)))

      val ex = Executors.newCachedThreadPool()

      val props = Seq(
        forAll(gl) {
          case (list, i) =>
            Process.runLog(ex)(Process(list: _*).take(i)).toList == list.take(i)
        },
        forAll(gl) {
          case (list, i) =>
            Process.runLog(ex)(Process(list: _*).drop(i)).toList == list.drop(i)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).filter(_ % 2 == 0))
            .toList == list.filter(_ % 2 == 0)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .dropWhile(_ % 2 == 0)
            .toList == list.dropWhile(_ % 2 == 0)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .dropWhile(_ => false)
            .toList == list
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .dropWhile(_ => true)
            .toList == Nil
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .takeWhile(_ % 2 == 0)
            .toList == list.takeWhile(_ % 2 == 0)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .takeWhile(_ => false)
            .toList == Nil
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*))
            .takeWhile(_ => true)
            .toList == list
        },
        forAll(Gen.int ** Gen.choose(0, 100000)) {
          case (x, n) =>
            Process.runLog(ex)(Process.constant(x).take(n)).toList == Vector
              .fill(n)(x)
        },
        forAll(g) { list =>
          val procs = Seq[Process[IO, Int]](
            Process(list: _*).filter(_ => throw Kill),
            Process(list: _*).takeWhile(_ => throw Kill),
            Process(list: _*).dropWhile(_ => throw Kill)
          )

          procs.forall { proc =>
            try {
              Process.runLog(ex)(proc)
              false
            } catch {
              case Kill         => true
              case _: Throwable => false
            }
          }
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).zipWithIndex)
            .toList == list.zipWithIndex
        },
        forAll(g ** g) {
          case (list1, list2) =>
            Process
              .runLog(ex)(Process(list1: _*).zip(Process(list2: _*)))
              .toList == list1.zip(list2)
        }
      )

      props.foreach(run(_))
    }
  }

}
