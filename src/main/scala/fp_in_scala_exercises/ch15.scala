package fp_in_scala_exercises
import ch13.{IO, Free}
import ch5.{Stream, Empty, Cons}
import ch7.nonblocking.Par
import ch8.{Gen, forAll, run, Prop}
import java.io.{BufferedReader, FileReader}

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
    def step(in: Option[I]): Result[I, O]

    def run(in: Stream[I]): Stream[O] =
      step(in.headOption) match {
        case Result(consumed, out, next) =>
          val nextIn = if (consumed) {
            in.drop(1)
          } else {
            in
          }
          val head = out.map(Stream(_)).getOrElse(Empty)
          head.append(next.map(_.run(nextIn)).getOrElse(Empty))
      }

    def chain[Q](after: Process[O, Q]): Process[I, Q] =
      after match {
        case Emit(head, tail) => Emit(head, chain(tail))
        case Await(recv) =>
          this match {
            case Emit(head, tail) => tail.chain(recv(Some(head)))
            case Await(recv0)     => Await(recv0(_).chain(after))
            case Halt()           => Halt().chain(recv(None))
          }
        case Halt() => Halt()
      }

    def |>[Q](after: Process[O, Q]): Process[I, Q] = chain(after)

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
            Emit(head, repeat0(tail))
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
  }

  object Process {
    def liftOne[A, B](f: A => B): Process[A, B] =
      await { a =>
        Emit(f(a), Halt())
      }

    def lift[A, B](f: A => B): Process[A, B] =
      await { a =>
        Emit(f(a), lift(f))
      }

    def lift2[A, B](f: A => B): Process[A, B] =
      liftOne(f).repeat

    def take[O](n: Int): Process[O, O] =
      if (n <= 0) {
        Halt()
      } else {
        await { (out: O) =>
          Emit(out, take(n - 1))
        }
      }

    def takeWhile[O](f: O => Boolean): Process[O, O] =
      Await[O, O] {
        case Some(out) if f(out) => Emit(out, takeWhile(f))
        case _                   => Halt()
      }

    def echo[O]: Process[O, O] =
      await { (out: O) =>
        Emit(out, echo)
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
            Emit(out, echo)
          }
        case _ => echo
      }

    def filter[O](f: O => Boolean): Process[O, O] =
      await { (out: O) =>
        if (f(out)) {
          Emit(out, filter(f))
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
          Emit((out, start), zipWithIndex0(start + 1))
        }
      zipWithIndex0(0)
    }

    def duplicate[O](n: Int): Process[O, O] = {
      def duplicateElem(k: Int, out: O): Process[O, O] =
        if (k <= 0) {
          duplicate(n)
        } else {
          Emit(out, duplicateElem(k - 1, out))
        }

      await { out => duplicateElem(n, out) }
    }

    def duplicate2[O](n: Int): Process[O, O] =
      if (n <= 0) {
        Halt()
      } else {
        await { (out: O) =>
          0.until(n).foldLeft[Process[O, O]](Halt())((p, _) => Emit(out, p))
        }.repeat
      }

    def count[I]: Process[I, Int] = {
      def count0(count: Int): Process[I, Int] =
        await { _ =>
          Emit(count, count0(count + 1))
        }

      count0(0)
    }

    def count2[I]: Process[I, Int] =
      loop(0)((in, count) => (count, count + 1))

    def sum: Process[Double, Double] = {
      def sum0(s: Double): Process[Double, Double] =
        await { value =>
          Emit(s + value, sum0(s + value))
        }

      sum0(0)
    }

    def sum2: Process[Double, Double] =
      loop(0.0)((in, sum) => (sum + in, sum + in))

    def mean: Process[Double, Double] = {
      def mean0(s: Double, count: Int): Process[Double, Double] =
        await { value =>
          Emit((s + value) / count + 1, mean0(s + value, count + 1))
        }

      mean0(0, 0)
    }

    def mean2: Process[Double, Double] =
      loop((0.0, 0)) {
        case (in, (sum, count)) =>
          ((sum + in) / (count + 1), (sum + in, count + 1))
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
          Emit(out, loop0(s2))
        }

      loop0(s)
    }
  }

  case class Result[-I, +O](
      consumed: Boolean,
      out: Option[O],
      next: Option[Process[I, O]]
  )

  case class Emit[-I, +O](head: O, tail: Process[I, O] = Halt())
      extends Process[I, O] {
    def step(in: Option[I]): Result[I, O] =
      Result(false, Some(head), Some(tail))
  }

  case class Await[-I, +O](recv: Option[I] => Process[I, O])
      extends Process[I, O] {
    def step(in: Option[I]): Result[I, O] =
      Result(!in.isEmpty, None, Some(recv(in)))
  }

  case class Halt() extends Process[Any, Nothing] {
    def step(in: Option[Any]): Result[Any, Nothing] = Result(false, None, None)
  }

  def testProcess: Unit = {
    val g = Gen.int.listOfN(Gen.choose(0, 100))
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
      }
    )

    props.foreach(run(_))
  }
}
