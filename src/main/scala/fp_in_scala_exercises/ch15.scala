package fp_in_scala_exercises
import ch12.Monad
import ch13.{IO, Free}
import ch5.{Stream, Empty, Cons}
import ch7.nonblocking.Par
import ch8.{Gen, forAll, run, Prop}
import java.io.{BufferedReader, FileReader}
import java.io.BufferedWriter
import java.io.FileWriter
import java.util.concurrent.{Executors, ExecutorService}
import scala.annotation.tailrec
import scala.collection.BufferedIterator
import scala.collection.mutable.ArrayBuilder

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

    def filter[O](f: O => Boolean): Process[O, O] = {
      lazy val filter: Process[O, O] =
        await { (out: O) =>
          if (f(out)) {
            emit(out, filter)
          } else {
            filter
          }
        }
      filter
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

    def exists[I](f: I => Boolean): Process[I, Boolean] = {
      lazy val exists: Process[I, Boolean] =
        Await {
          case Some(in) =>
            if (f(in)) {
              emit(true)
            } else {
              exists
            }
          case None => emit(false)
        }
      exists
    }
  }

  case class Emit[-I, +O](head: O, tail: () => Process[I, O] = () => Halt())
      extends Process[I, O]

  case class Await[-I, +O](recv: Option[I] => Process[I, O])
      extends Process[I, O]

  case class Halt() extends Process[Any, Nothing]

  def testProcess: Unit = {
    val g = Gen.int.listOfN(Gen.choose(0, 10000))
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
          .toList == list
          .foldLeft(List.empty[Double]) { (sums, x) =>
            (sums.headOption.getOrElse(0.0) + x) :: sums
          }
          .reverse
      },
      forAll(g.map(_.map(_.toDouble))) { list =>
        Seq(
          Process.mean,
          Process.mean2,
          Process.mean3
        ).forall(
          _.run(Stream(list: _*)).toList ==
            list
              .foldLeft(List.empty[Double]) { (sums, x) =>
                (sums.headOption.getOrElse(0.0) + x) :: sums
              }
              .reverse
              .zipWithIndex
              .map {
                case (sum, i) => sum / (i + 1)
              }
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
        Process(0.until(10000): _*).run(Stream()).toList == 0
          .until(10000)
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
        Process
          .exists((y: Int) => true)
          .run(Stream(input: _*))
          .toList == List(!input.isEmpty) &&
        Process
          .exists((y: Int) => false)
          .run(Stream(input: _*))
          .toList == List(false) &&
        Process
          .exists((y: Int) => y % 2 == 0)
          .run(Stream(input: _*))
          .toList == List(input.exists(_ % 2 == 0))
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
      import Process.{tryHalt, await, emit1, emit}

      def onHalt[O2 >: O](
          f: Throwable => Process[F, O2]
      ): Process[F, O2] =
        this match {
          case Emit(head, tail) =>
            Emit(head, tail.onHalt(f(_)))
          case Await(req, recv) =>
            await(req)(recv(_).onHalt(f(_)))
          case Halt(err) =>
            tryHalt(f(err))
        }

      def ++[O2 >: O](after: => Process[F, O2]): Process[F, O2] =
        onHalt {
          case End => after
          case err => Halt(err)
        }

      def onComplete[O2 >: O](after: => Process[F, O2]): Process[F, O2] =
        onHalt {
          case End => after.asFinalizer
          case err => after.asFinalizer ++ Halt(err)
        }

      def asFinalizer: Process[F, O] =
        this match {
          case Emit(head, tail) =>
            Emit(head, tail.asFinalizer)
          case Await(req, recv) =>
            await(req) {
              case Left(Kill) => asFinalizer
              case x          => recv(x)
            }
          case Halt(err) => Halt(err)
        }

      def map[O2](f: O => O2): Process[F, O2] =
        this match {
          case Emit(head, tail) =>
            tryHalt(Emit(f(head), tail.map(f(_))))

          case Await(req, recv) =>
            await(req)(recv(_).map(f(_)))

          case Halt(err) => Halt(err)
        }

      def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
        this match {
          case Emit(head, tail) => tryHalt(f(head)) ++ tail.flatMap(f(_))
          case Await(req, recv) => await(req)(recv(_).flatMap(f(_)))
          case Halt(err)        => Halt(err)
        }

      def repeat: Process[F, O] =
        this ++ repeat

      def filter(f: O => Boolean): Process[F, O] =
        this match {
          case Emit(head, tail) =>
            val rest = tail.filter(f(_))
            tryHalt {
              if (f(head)) {
                Emit(head, rest)
              } else {
                rest
              }
            }
          case Await(req, recv) =>
            await(req)(recv(_).filter(f(_)))
          case Halt(err) => Halt(err)
        }

      def take(n: Int): Process[F, O] =
        if (n <= 0) {
          Halt(End)
        } else {
          this match {
            case Emit(head, tail) => Emit(head, tail.take(n - 1))
            case Await(req, recv) => await(req)(recv(_).take(n - 1))
            case Halt(err)        => Halt(err)
          }
        }

      def takeWhile(f: O => Boolean): Process[F, O] =
        this match {
          case Emit(head, tail) =>
            tryHalt {
              if (f(head)) {
                Emit(head, tail.takeWhile(f))
              } else {
                Halt(End)
              }
            }
          case Await(req, recv) =>
            await(req)(recv(_).takeWhile(f))
          case Halt(err) => Halt(err)
        }

      def drop(n: Int): Process[F, O] =
        if (n <= 0) {
          this
        } else {
          this match {
            case Emit(head, tail) => tail.drop(n - 1)
            case Await(req, recv) => await(req)(recv(_).drop(n - 1))
            case Halt(err)        => Halt(err)
          }
        }

      def dropWhile(f: O => Boolean): Process[F, O] =
        this match {
          case Emit(head, tail) =>
            tryHalt {
              if (f(head)) {
                tail.dropWhile(f(_))
              } else {
                this
              }
            }
          case Await(req, recv) => await(req)(recv(_).dropWhile(f(_)))
          case Halt(err)        => Halt(err)
        }

      def exists(f: O => Boolean): Process[F, Boolean] =
        this match {
          case Emit(head, tail) =>
            tryHalt {
              if (f(head)) {
                Emit(true, Halt(End))
              } else {
                tail.exists(f(_))
              }
            }
          case Await(req, recv) => await(req)(recv(_).exists(f(_)))
          case Halt(err)        => Emit(false, Halt(err))
        }

      def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
        val out = IndexedSeq.newBuilder[O]

        def runLog0(p: Process[F, O]): F[IndexedSeq[O]] =
          p match {
            case Emit(head, tail) =>
              out.addOne(head)
              runLog0(tail)
            case Await(req, recv) =>
              F.flatMap(F.attempt(req))(x => runLog0(tryHalt(recv(x))))
            case Halt(End) => F.unit(out.result())
            case Halt(err) => F.fail(err)
          }
        runLog0(this)
      }

      def |>[O2](p: Process1[O, O2]): Process[F, O2] =
        p match {
          case Emit(head, tail) => emit(head, this |> tail)
          case Await(req, recv) =>
            this match {
              case Emit(head, tail)   => tail |> tryHalt(recv(Right(head)))
              case Await(req2, recv2) => await(req2)(recv2(_) |> p)
              case Halt(err)          => Halt(err) |> recv(Left(err))
            }
          case Halt(err) => this.kill.onHalt(err2 => Halt(err) ++ Halt(err2))
        }

      def pipe[O2](p: Process1[O, O2]): Process[F, O2] = this |> p

      @tailrec
      final def kill[O2]: Process[F, O2] =
        this match {
          case Emit(head, tail) => tail.kill
          case Await(req, recv) =>
            recv(Left(Kill)).drain.onHalt {
              case Kill => Halt(End)
              case err  => Halt(err)
            }
          case Halt(err) => Halt(err)
        }

      def drain[O2]: Process[F, O2] =
        this match {
          case Emit(head, tail) => tail.drain
          case Await(req, recv) => await(req)(recv(_).drain)
          case Halt(err)        => Halt(err)
        }

      def tee[O2, O3](p: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] =
        t match {
          case Emit(head, tail) => emit(head, tee(p)(tail))
          case Await(req, recv) =>
            req.get match {
              case Left(o) =>
                this match {
                  case Emit(head, tail) =>
                    tail.tee(p)(tryHalt(recv(Right(o(head)))))
                  case Await(req2, recv2) =>
                    await(req2)(x => tryHalt(recv2(x)).tee(p)(t))
                  case Halt(err) => p.kill.onComplete(Halt(err))
                }
              case Right(o2) =>
                p match {
                  case Emit(head, tail) =>
                    this.tee(tail)(tryHalt(recv(Right(o2(head)))))
                  case Await(req2, recv2) =>
                    await(req2)(x => this.tee(tryHalt(recv2(x)))(t))
                  case Halt(err) => this.kill.onComplete(Halt(err))
                }
            }
          case Halt(err) =>
            this.kill.onHalt(err2 =>
              p.kill.onHalt(err3 => Halt(err) ++ Halt(err2) ++ Halt(err3))
            )
        }

      def to(sink: Sink[F, O]): Process[F, Unit] =
        Process.join(this.tee(sink)(Process.zipWith((x, f) => f(x))))

      def through[O2](sink: Process[F, O => Process[F, O2]]): Process[F, O2] =
        Process.join(this.tee(sink)(Process.zipWith((x, f) => f(x))))
    }

    case class Is[I]() {
      sealed trait f[X]
      val Get = new f[I] {}
    }

    type Process1[I, O] = Process[Is[I]#f, O]

    case class T[I, I2]() {
      sealed trait f[X] { def get: Either[I => X, I2 => X] }
      val L = new f[I] { def get = Left(identity[I](_)) }
      val R = new f[I2] { def get = Right(identity[I2](_)) }
    }

    type Tee[I, I2, O] = Process[T[I, I2]#f, O]
    def L[I, I2]: T[I, I2]#f[I] = T().L
    def R[I, I2]: T[I, I2]#f[I2] = T().R

    type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

    type Channel[F[_], I, O] = Process[F, I => Process[F, O]]

    object Process {
      def apply[F[_], O](xs: O*): Process[F, O] =
        if (xs.isEmpty) {
          Halt(End)
        } else {
          Emit(xs.head, apply(xs.tail: _*))
        }

      def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] =
        p.flatMap(x => x)

      def tryHalt[F[_], O](f: => Process[F, O]): Process[F, O] =
        try {
          f
        } catch {
          case err: Throwable => Halt(err)
        }

      def await[F[_], O, A](req: F[A])(
          recv: Either[Throwable, A] => Process[F, O]
      ): Process[F, O] = Await(req, recv)

      def resource[R, O](acquire: IO[R])(
          use: R => Process[IO, O]
      )(release: R => Process[IO, O]): Process[IO, O] =
        await[IO, O, R](acquire) {
          case Right(r)  => use(r).onComplete(release(r))
          case Left(err) => Halt(err)
        }

      def eval[F[_], A](a: F[A]): Process[F, A] =
        await(a) {
          case Right(a)  => Emit(a, Halt(End))
          case Left(err) => Halt(err)
        }

      def eval_[F[_], A, B](a: F[A]): Process[F, B] =
        await(a) {
          case Right(_)  => Halt(End)
          case Left(err) => Halt(err)
        }

      def runLog[O](
          ex: ExecutorService
      )(src: Process[IO, O]): IndexedSeq[O] = {
        val out = IndexedSeq.newBuilder[O]

        @tailrec
        def runLog0(src: Process[IO, O]): IndexedSeq[O] = {
          src match {
            case Emit(head, tail) =>
              out.addOne(head)
              runLog0(tail)
            case Await(req, recv) =>
              val msg =
                try {
                  Right(Par.run(ex)(Free.runPar(req)))
                } catch {
                  case err: Throwable => Left(err)
                }
              runLog0(tryHalt(recv(msg)))
            case Halt(End) => out.result()
            case Halt(err) => throw err
          }
        }

        runLog0(src)
      }

      def emit[F[_], O](
          head: O,
          tail: Process[F, O] = Halt[F, O](End)
      ): Process[F, O] =
        Emit(head, tail)

      def await1[I, O](
          recv: I => Process1[I, O],
          fallback: => Process1[I, O] = halt1[I, O](End)
      ): Process1[I, O] =
        await(Is[I]().Get) {
          _ match {
            case Right(in) => tryHalt(recv(in))
            case Left(End) => fallback
            case Left(err) => Halt(err)
          }
        }

      def emit1[I, O](
          head: O,
          tail: Process1[I, O] = Halt[Is[I]#f, O](End)
      ): Process1[I, O] =
        emit(head, tail)

      def halt1[I, O](err: Throwable): Process1[I, O] = Halt(err)

      def liftOne[I, O](f: I => O): Process1[I, O] =
        await1[I, O](in => emit1(f(in)))

      def lift[I, O](f: I => O): Process1[I, O] =
        liftOne(f).repeat

      def filter[I](f: I => Boolean): Process1[I, I] =
        await1[I, I] { in =>
          if (f(in)) {
            emit1(in)
          } else {
            halt1(End)
          }
        }.repeat

      def take[I](n: Int): Process1[I, I] =
        await1[I, I] { in => Emit(in, take(n - 1)) }

      def echo[I]: Process1[I, I] = {
        lazy val echo: Process1[I, I] = await1[I, I] { in => Emit(in, echo) }
        echo
      }

      def drop[I](n: Int): Process1[I, I] =
        if (n <= 0) {
          echo
        } else {
          await1[I, I] { in => drop(n - 1) }
        }

      def takeWhile[I](f: I => Boolean): Process1[I, I] = {
        lazy val take: Process1[I, I] = await1 { in =>
          if (f(in)) {
            Emit(in, take)
          } else {
            halt1(End)
          }
        }
        take
      }

      def dropWhile[I](f: I => Boolean): Process1[I, I] = {
        lazy val drop: Process1[I, I] = await1 { in =>
          if (f(in)) {
            drop
          } else {
            echo
          }
        }
        drop
      }

      def haltT[I, I2, O](err: Throwable): Tee[I, I2, O] =
        Halt(err)

      def awaitL[I, I2, O](
          recv: I => Tee[I, I2, O],
          fallback: => Tee[I, I2, O] = haltT[I, I2, O](End)
      ): Tee[I, I2, O] =
        await(L[I, I2]) {
          case Right(in) => tryHalt(recv(in))
          case Left(End) => fallback
          case Left(err) => Halt(err)
        }

      def awaitR[I, I2, O](
          recv: I2 => Tee[I, I2, O],
          fallback: => Tee[I, I2, O] = haltT[I, I2, O](End)
      ): Tee[I, I2, O] =
        await(R[I, I2]) {
          case Right(in) => tryHalt(recv(in))
          case Left(End) => fallback
          case Left(err) => Halt(err)
        }

      def emitT[I, I2, O](
          head: O,
          tail: Tee[I, I2, O] = haltT(End)
      ): Tee[I, I2, O] =
        emit(head, tail)

      def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = {
        lazy val zip: Tee[I, I2, O] = awaitL { left =>
          awaitR { right =>
            Emit(f(left, right), zip)
          }
        }
        zip.repeat
      }

      def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

      def fileW(filename: String, append: Boolean = false): Sink[IO, String] =
        resource(IO(new BufferedWriter(new FileWriter(filename, append)))) {
          file =>
            eval(IO { (s: String) =>
              eval(IO(file.write(s + "\n")))
            }).repeat
        }(file => eval_(IO(file.close())))

      def constant[A](a: A): Process[IO, A] =
        eval[IO, A](IO(a)).repeat
    }

    case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

    case class Await[F[_], O, A](
        req: F[A],
        recv: Either[Throwable, A] => Process[F, O]
    ) extends Process[F, O]

    case class Halt[F[_], O](err: Throwable) extends Process[F, O]

    case object End extends Exception
    case object Kill extends Exception

    import Process.await

    def readLines(filename: String): Process[IO, String] =
      await(
        IO(io.Source.fromFile(filename))
      ) {
        case Right(reader) =>
          val lines = reader.getLines()
          lazy val read: Process[IO, String] =
            Process
              .eval(IO {
                if (lines.hasNext) {
                  Some(lines.next())
                } else {
                  None
                }
              })
              .flatMap {
                case Some(line) => Emit(line, read)
                case None       => Halt(End)
              }
          read.onComplete {
            Process.eval_(IO { reader.close() })
          }
        case Left(err) => Halt(err)
      }

    def testProcess: Unit = {
      val g = Gen.int.listOfN(Gen.choose(0, 1000))
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
            .runLog(ex)(Process(list: _*).dropWhile(_ % 2 == 0))
            .toList == list.dropWhile(_ % 2 == 0)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).dropWhile(_ => false))
            .toList == list
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).dropWhile(_ => true))
            .toList == Nil
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(
              Process(list: _*).takeWhile(_ % 2 == 0)
            )
            .toList == list.takeWhile(_ % 2 == 0)
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(
              Process(list: _*)
                .takeWhile(_ => false)
            )
            .toList == Nil
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(
              Process(list: _*)
                .takeWhile(_ => true)
            )
            .toList == list
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
          Process.runLog(ex)(Process(list: _*)) == list.toIndexedSeq
        },
        forAll(g) { list =>
          Process.runLog(ex)(
            Process(list: _*).onHalt(Halt[IO, Int](_))
          ) == list.toIndexedSeq
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).exists(_ => true))
            .toList == List(!list.isEmpty) &&
          Process
            .runLog(ex)(Process(list: _*).exists(_ => false))
            .toList == List(false) &&
          Process
            .runLog(ex)(Process(list: _*).exists(_ % 2 == 0))
            .toList == List(list.exists(_ % 2 == 0))
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(
              Process(list: _*).flatMap(x => Process(List.fill(10)(x): _*))
            )
            .toList == list.flatMap(List.fill(10)(_))
        },
        forAll(g) { list =>
          Process
            .runLog(ex)(Process(list: _*).map(_ * 13))
            .toList == list.map(_ * 13)
        }
      )

      props.foreach(run(_))
    }

    trait MonadCatch[F[_]] extends Monad[F] {
      def attempt[A](a: F[A]): F[Either[Throwable, A]]
      def fail[A](t: Throwable): F[A]
    }

    def lines(filename: String): Process[IO, String] = {
      Process.resource(IO(io.Source.fromFile(filename))) { source =>
        val iter = source.getLines()

        lazy val p: Process[IO, String] =
          Process
            .eval(IO {
              if (iter.hasNext) {
                Some(iter.next())
              } else {
                None
              }
            })
            .flatMap {
              case Some(line) => Emit(line, p)
              case None       => Halt(End)
            }

        p
      }(source => Process.eval_(IO(source.close())))
    }

    def fahrenheit: Process[IO, Unit] = {
      val out = for {
        fs <- lines("fahrenheit.txt").tee(lines("fahrenheit2.txt"))(Process.zip)
      } yield ((fs._1.toDouble + fs._2.toDouble - 32) * 5 / 9).toString()
      out.to(Process.fileW("celsius.txt", true))
    }

    def fahrenheit2: Process[IO, Unit] = {
      lines("fahrenheit.txt").flatMap { filename =>
        lines(filename)
          .map(s => ((s.toDouble - 32) * 5 / 9).toString())
          .through(Process.fileW(filename.takeWhile(_ != '.') + ".cel"))
      }
    }
  }
}
