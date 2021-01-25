package fp_in_scala_exercises
import ch13.{IO, Free}
import ch12.Monad
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
    import Process.emit

    def step(in: Option[I]): Result[I, O] =
      this match {
        case Emit(head, tail) => Result(false, Some(head), Some(tail()))
        case Await(recv)      => Result(!in.isEmpty, None, Some(recv(in)))
        case Halt()           => Result(false, None, None)
      }

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

    def branch[I2 <: I, O2](other: Process[I2, O2]): Process[I2, (O, O2)] =
      this match {
        case Emit(head, tail) =>
          other match {
            case Emit(head2, tail2) =>
              emit((head, head2), tail().branch(tail2()))
            case Await(recv) => Await(in => tail().branch(recv(in)))
            case Halt()      => Halt()
          }
        case Await(recv) =>
          other match {
            case Emit(head, tail) => this.branch(tail())
            case Await(recv2)     => Await(in => recv(in).branch(recv2(in)))
            case Halt()           => Halt()
          }
        case Halt() => Halt()
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
      sum.branch(count).map {
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

  case class Result[-I, +O](
      consumed: Boolean,
      out: Option[O],
      next: Option[Process[I, O]]
  )

  case class Emit[-I, +O](head: O, tail: () => Process[I, O] = () => Halt())
      extends Process[I, O]

  case class Await[-I, +O](recv: Option[I] => Process[I, O])
      extends Process[I, O]

  case class Halt() extends Process[Any, Nothing]

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
      }
    )

    props.foreach(run(_))
  }
}
