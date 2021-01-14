package fp_in_scala_exercises
import ch8.{Gen, SGen, Prop, forAll}
import scala.util.matching.Regex
import _root_.fp_in_scala_exercises.ch8.tests.par
import scala.annotation.tailrec

object ch9 {
  case class ParseError(stack: List[(Location, String)]) {
    def push(location: Location, message: String) =
      copy((location, message) :: stack)

    def label(message: String) =
      copy(List((lastLocation, message)))

    def lastLocation: Location =
      stack.last._1
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset).count(_ == '\n') + 1
    lazy val col = if (offset == 0) {
      1
    } else {
      offset - input.lastIndexWhere(_ == '\n', offset - 1)
    }

    def subInput: String = input.substring(offset)

    def advance(length: Int): Location = copy(offset = offset + length)

    def toError(message: String): ParseError = ParseError(List((this, message)))
  }

  trait Parsers[Parser[+_]] { self =>
    implicit def string(s: String): Parser[String]

    def charClass(name: String)(f: Char => Boolean): Parser[Char]

    implicit def regex(r: Regex): Parser[String]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    implicit def asStringParser[A](a: A)(implicit
        f: A => Parser[String]
    ): ParserOps[String] = ParserOps(f(a))

    def succeed[A](a: A): Parser[A]

    def fail(message: String): Parser[Nothing]

    def attempt[A](p: Parser[A]): Parser[A]

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def label[A](name: String)(p: Parser[A]): Parser[A]

    def scope[A](name: String)(p: Parser[A]): Parser[A]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def char(ch: Char): Parser[Char] =
      string(ch.toString).map(_.charAt(0))

    def chars(chs: Char*): Parser[Char] = {
      val name = chs.map(ch => s"'$ch'").mkString(" | ")
      charClass(name)(chs.contains(_))
    }

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      flatMap(p) { a => succeed(f(a)) }

    def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C) =
      flatMap(p1) { a => map(p2)(f(a, _)) }

    def fold[A, B](p: Parser[A])(z: B)(f: (B, A) => B): Parser[B] =
      or(flatMap(p) { a => fold(p)(f(z, a))(f) }, succeed(z))

    def foldRight[A, B](p: Parser[A])(z: B)(f: (A, B) => B): Parser[B] =
      or(map2(p, foldRight(p)(z)(f))(f), succeed(z))

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      map2(p1, p2)((_, _))

    def optional[A](p: Parser[A]): Parser[Option[A]] =
      or(p.map(Some(_)), succeed(None))

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) {
        succeed(Nil)
      } else {
        map2(p, listOfN(n - 1, p))(_ :: _)
      }

    def many[A](p: Parser[A]): Parser[List[A]] =
      foldRight(p)(List.empty[A]) { (a, as) => a :: as }

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map(product(p, many(p))) {
        case (a, as) => a :: as
      }

    def bookend[A, B, C](left: Parser[A], right: Parser[B])(
        p: Parser[C]
    ): Parser[C] =
      product(product(left, p), right).map {
        case ((_, c), _) => c
      }

    def manySep[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
      map(product(p, many(product(sep, p).map(_._2)))) {
        case (a, as) => a :: as
      } | succeed(Nil)

    def count[A](p: Parser[A]): Parser[Int] =
      fold(p)(0)((count, _) => count + 1)

    def slice[A](p: Parser[A]): Parser[String]

    def digitAs: Parser[String] =
      regex("[0-9]".r).flatMap { s =>
        slice(listOfN(s.toInt, char('a')))
      }

    def discard[A](p: Parser[A]): Parser[Unit] = map(p)(_ => ())

    val discardWhitespace: Parser[Unit] = {
      def isWhitespace(char: Char): Boolean =
        char match {
          case ' ' | '\t' | '\r' | '\n' => true
          case _                        => false
        }
      fold(charClass("whitespace")(isWhitespace(_)))(())((_, _) => ())
    }

    val number: Parser[Double] = {
      val nonzeroDigit = charClass("nonzero digit")('1'.to('9').contains(_))

      val digit = charClass("digit")('0'.to('9').contains(_))

      val discardDigits =
        fold(digit)(())((_, _) => ())

      val integer =
        (optional(char('-')) ** (char('0') | (nonzeroDigit ** discardDigits)))
          .label("Invalid integer part")

      val fractionalPart =
        (char('.') ** digit ** discardDigits).label("Invalid fractional part")

      val exponent =
        ((chars('e', 'E')) **
          optional(chars('+', '-')) ** digit ** discardDigits)
          .label("Invalid exponent")

      (integer ** optional(fractionalPart) ** optional(exponent)).slice
        .map(_.toDouble)
        .scope("number")
    }

    val escapedString: Parser[String] = bookend('"', '"') {
      val simpleEscapeCode = Seq(
        '"' -> '\"',
        '\\' -> '\\',
        '/' -> '/',
        'b' -> '\b',
        'f' -> '\f',
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t'
      ).map {
        case (code, ch) => char(code).map(_ => ch)
      }.reduce(_ | _)

      val hexDigit = charClass("hexadecimal digit") { ch =>
        '0'.to('9').contains(ch) ||
        'a'.to('f').contains(ch) ||
        'A'.to('F').contains(ch)
      }

      val uEscapeCode =
        (char('u') >* listOfN(4, hexDigit).slice).map(_.toInt.toChar)

      val badEscapeSequence =
        fail("bad escape sequence")

      val escape =
        char('\\') >* (simpleEscapeCode | uEscapeCode | badEscapeSequence)

      val regularChar =
        charClass("regular character") { char =>
          !char.isControl && char != '"' && char != '\\'
        }

      val unexpectedControlChar =
        fail("unexpected control character")

      fold(regularChar | escape | unexpectedControlChar)("")(_ + _)
        .map(_.mkString)
    }

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def *(): Parser[List[A]] = self.many(p)
      def +(): Parser[List[A]] = self.many1(p)
      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def <*[B](p2: => Parser[B]): Parser[A] = **(p2).map(_._1)
      def >*[B](p2: => Parser[B]): Parser[B] = **(p2).map(_._2)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def slice: Parser[String] = self.slice(p)
      def discard: Parser[Unit] = self.discard(p)
      def bookend[B, C](left: Parser[B], right: Parser[C]): Parser[A] =
        self.bookend(left, right)(p)
      def manySep[B](sep: Parser[B]): Parser[List[A]] = self.manySep(p, sep)
      def label(name: String): Parser[A] = self.label(name)(p)
      def scope(name: String): Parser[A] = self.scope(name)(p)
      def run(input: String): Either[ParseError, A] = self.run(p)(input)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def charLaw: Prop =
        forAll(Gen.choose(32, 127).map(_.toChar))(c =>
          run(char(c))(c.toString) == Right(c)
        )

      def succeedLaw(in: Gen[String]): Prop =
        forAll(Gen.int ** in) {
          case (x, s) => run(succeed(x))(s) == Right(x)
        }

      def productLaw[A, B](p1: Parser[A], p2: Parser[B])(
          in: Gen[String]
      ): Prop =
        forAll(in) { s =>
          val result = run(p1 ** p2)(s)
          run(p1)(s) match {
            case Right(a) =>
              val s2 = s.substring(run(p1.slice.map(_.length))(s).right.get)
              run(p2)(s2) match {
                case Right(b)    => result == Right((a, b))
                case Left(error) => result == Left(error)
              }
            case Left(error) => result == Left(error)
          }
        }

      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
        forAll(inputs ** SGen.string) {
          case (input, msg) =>
            run(label(msg)(p))(input) match {
              case Left(error) => error.stack.head._2 == msg
              case Right(_)    => true
            }
        }
    }
  }

  sealed trait JSON

  object JSON {
    case object JNull extends JSON
    case class JString(get: String) extends JSON
    case class JNumber(get: Double) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  def jsonParser[Parser[+_]](p: Parsers[Parser]): Parser[JSON] = {
    import p._
    import JSON._

    def literal: Parser[JSON] =
      Seq(
        string("null").map(_ => JNull),
        number.map(s => JNumber(s.toDouble)),
        escapedString.map(JString(_)),
        string("true").map(_ => JBool(true)),
        string("false").map(_ => JBool(false))
      ).reduce(_ | _).scope("literal")

    def array: Parser[JSON] =
      bookend("[" ** discardWhitespace, discardWhitespace ** "]") {
        element.manySep(",").map { elems =>
          JArray(elems.toIndexedSeq)
        }
      }.scope("array")

    def obj: Parser[JSON] = {
      val key = (discardWhitespace >* escapedString <* discardWhitespace)
      bookend("{" ** discardWhitespace, discardWhitespace ** "}") {
        (key ** (char(':') >* element))
          .manySep(",")
          .map(pairs => JObject(pairs.toMap))
      }.scope("object")
    }

    def element: Parser[JSON] =
      (discardWhitespace >* (literal | array | obj) <* discardWhitespace)

    discardWhitespace >* array | obj <* discardWhitespace
  }

  sealed trait ParseResult[+A] {
    def flatMap[B](f: (A, Int) => ParseResult[B]): ParseResult[B]

    def mapValue[B](f: A => B): ParseResult[B] =
      flatMap((a, offset) => Succeeded(f(a), offset))

    def mapError(f: ParseError => ParseError): ParseResult[A]

    def toEither: Either[ParseError, A]
  }

  case class Succeeded[A](a: A, length: Int) extends ParseResult[A] {
    def flatMap[B](f: (A, Int) => ParseResult[B]): ParseResult[B] =
      f(a, length)

    def mapError(f: ParseError => ParseError): ParseResult[A] = this

    def toEither: Either[ParseError, A] = Right(a)
  }

  case class FailedUncommitted(error: ParseError) extends ParseResult[Nothing] {
    def flatMap[B](
        f: (Nothing, Int) => ParseResult[B]
    ): ParseResult[Nothing] = this

    def mapError(f: ParseError => ParseError): ParseResult[Nothing] =
      FailedUncommitted(f(error))

    def toEither: Either[ParseError, Nothing] = Left(error)
  }

  case class FailedCommitted(error: ParseError) extends ParseResult[Nothing] {
    def flatMap[B](
        f: (Nothing, Int) => ParseResult[B]
    ): ParseResult[Nothing] = this

    def mapError(f: ParseError => ParseError): ParseResult[Nothing] =
      FailedCommitted(f(error))

    def toEither: Either[ParseError, Nothing] = Left(error)
  }

  sealed trait MyParser[+A] {
    def parse(location: Location): ParseResult[A]
    def slice(location: Location): ParseResult[String]
  }

  object MyParser extends Parsers[MyParser] {
    implicit def string(s: String): MyParser[String] =
      new MyParser[String] {
        def parse(location: Location): ParseResult[String] = {
          val subInput = location.subInput

          val commonPrefix = subInput.zip(s).takeWhile {
            case (ch1, ch2) => ch1 == ch2
          }

          if (commonPrefix.length == s.length) {
            Succeeded(s, s.length)
          } else {
            val ch = if (commonPrefix.length >= subInput.length) {
              "<eof>"
            } else {
              subInput.charAt(commonPrefix.length).toString
            }

            val error = location.toError(
              s"Expected $s, but got ${commonPrefix.mkString}$ch"
            )

            if (commonPrefix.isEmpty) {
              FailedUncommitted(error)
            } else {
              FailedCommitted(error)
            }
          }
        }

        def slice(location: Location): ParseResult[String] =
          parse(location)
      }

    def charClass(name: String)(f: Char => Boolean): MyParser[Char] =
      new MyParser[Char] {
        def parse(location: Location): ParseResult[Char] = {
          val subInput = location.subInput

          subInput.headOption
            .filter(f)
            .map(Succeeded(_, 1))
            .getOrElse {
              val ch = subInput.headOption.map(_.toString).getOrElse("<eof>")
              val error = location.toError(s"Expected a $name, but got $ch")
              FailedUncommitted(error)
            }
        }

        def slice(location: Location): ParseResult[String] =
          parse(location).mapValue(_.toString)
      }

    implicit def regex(re: Regex): MyParser[String] =
      new MyParser[String] {
        def parse(location: Location): ParseResult[String] = {
          val subInput = location.subInput
          re.findPrefixMatchOf(subInput)
            .map { mtch =>
              val s = mtch.matched
              Succeeded(s, s.length)
            }
            .getOrElse {
              val ch = subInput.headOption.map(_.toString).getOrElse("<eof>")
              val error = location.toError(
                s"Expected a token matching ${re.pattern}, but got $ch"
              )
              FailedUncommitted(error)
            }
        }

        def slice(location: Location): ParseResult[String] =
          parse(location)
      }

    def succeed[A](a: A): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          Succeeded(a, 0)

        def slice(location: Location): ParseResult[String] =
          Succeeded("", 0)
      }

    def fail(message: String): MyParser[Nothing] =
      new MyParser[Nothing] {
        def parse(location: Location): ParseResult[Nothing] =
          FailedUncommitted(location.toError(message))

        def slice(location: Location): ParseResult[String] =
          FailedUncommitted(location.toError(message))
      }

    def attempt[A](p: MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p.parse(location) match {
            case FailedCommitted(error) => FailedUncommitted(error)
            case res                    => res
          }

        def slice(location: Location): ParseResult[String] =
          p.slice(location) match {
            case FailedCommitted(error) => FailedUncommitted(error)
            case res                    => res
          }
      }

    def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p1.parse(location) match {
            case FailedUncommitted(_) => p2.parse(location)
            case res                  => res
          }

        def slice(location: Location): ParseResult[String] =
          p1.slice(location) match {
            case FailedUncommitted(_) => p2.slice(location)
            case res                  => res
          }
      }

    def label[A](name: String)(p: MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p.parse(location).mapError(_.label(name))

        def slice(location: Location): ParseResult[String] =
          p.slice(location).mapError(_.label(name))
      }

    def scope[A](name: String)(p: MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p.parse(location).mapError(_.push(location, name))

        def slice(location: Location): ParseResult[String] =
          p.slice(location).mapError(_.push(location, name))
      }

    def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] =
      new MyParser[B] {
        def parse(location: Location): ParseResult[B] = {
          p.parse(location).flatMap { (a, length) =>
            f(a).parse(location.advance(length)) match {
              case Succeeded(b, length2) => Succeeded(b, length + length2)
              case FailedUncommitted(error) if length > 0 =>
                FailedCommitted(error)
              case res => res
            }
          }
        }

        def slice(location: Location): ParseResult[String] = {
          p.parse(location).flatMap { (a, length) =>
            f(a).slice(location.advance(length)) match {
              case Succeeded(s, length2) =>
                Succeeded(location.subInput.take(length) + s, length + length2)
              case FailedUncommitted(error) if length > 0 =>
                FailedCommitted(error)
              case res => res
            }
          }
        }
      }

    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
      p.parse(Location(input)).toEither

    def slice[A](p: MyParser[A]): MyParser[String] =
      new MyParser[String] {
        def parse(location: Location): ParseResult[String] =
          p.slice(location)

        def slice(location: Location): ParseResult[String] =
          parse(location)
      }

    override def many[A](p: MyParser[A]): MyParser[List[A]] =
      new MyParser[List[A]] {
        def parse(location: Location): ParseResult[List[A]] = {
          fold(p.parse(_))(location)(List.empty[A])((as, a) => a :: as)
        }

        def slice(location: Location): ParseResult[String] = {
          fold(p.slice(_))(location)("")(_ + _)
        }

        def fold[X, B](f: Location => ParseResult[X])(
            location: Location
        )(z: B)(g: (B, A) => B): ParseResult[B] = {
          @tailrec
          def fold0(
              location: Location
          )(z: B)(f: (B, A) => B)(totalLength: Int): ParseResult[B] = {
            p.parse(location) match {
              case Succeeded(a, length) =>
                fold0(location.advance(length))(f(z, a))(f)(
                  totalLength + length
                )
              case res: FailedCommitted => res
              case FailedUncommitted(error) =>
                Succeeded(z, totalLength)
            }
          }

          fold0(location)(z)(g)(0)
        }
      }
  }
}
