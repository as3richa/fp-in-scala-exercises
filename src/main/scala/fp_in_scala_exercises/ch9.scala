package fp_in_scala_exercises
import ch8.{Gen, SGen, Prop, forAll}
import scala.util.matching.Regex
import scala.annotation.tailrec

object ch9 {
  sealed trait ParseError {
    def push(location: Location, message: String): ParseError =
      ParseError.Cons((location, message), this)

    def label(message: String): ParseError =
      ParseError.Nil.push(lastLocation, message)

    def lastLocation: Location

    def toList: List[(Location, String)]

    def pretty: String = {
      val byLine = toList
        .groupBy(_._1.line)
        .map {
          case (lineNum, pairs) =>
            val sorted = pairs.sortBy(_._1.col)
            val withPrev = sorted.zip(None +: sorted.map(Some(_)))

            val sourceLine = pairs.head._1.sourceLine

            val caretLine = withPrev.map {
              case ((location, message), prev) =>
                val spaces = location.col - 1 - prev.map(_._1.col).getOrElse(0)
                if (spaces < 0) {
                  ""
                } else {
                  " " * spaces + "^"
                }
            }.mkString

            val messageLines = withPrev.map {
              case ((location, message), prev) =>
                " " * (location.col - 1) + message
            }

            lineNum -> (sourceLine :: caretLine :: messageLines)
        }
        .toSeq
        .sortBy(_._1)

      byLine
        .zip(None +: byLine.map(Some(_)))
        .flatMap {
          case (lineNum -> lines, prev) =>
            val ellipsisLine =
              prev.filter(_._1 != lineNum - 1).map(_ => "...")

            val lineNumLine = s"  Line $lineNum"

            ellipsisLine ++ (lineNumLine :: lines)
        }
        .mkString("\n")
    }
  }

  object ParseError {
    case object Nil extends ParseError {
      def toList: List[(Location, String)] = List.empty[(Location, String)]

      def lastLocation: Location =
        throw new Error("lastLocation called on Nil ParseError")
    }

    case class Cons(head: (Location, String), tail: ParseError)
        extends ParseError {
      def toList: List[(Location, String)] = head :: tail.toList

      def lastLocation: Location =
        tail match {
          case Nil        => head._1
          case Cons(_, _) => tail.lastLocation
        }
    }
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset).count(_ == '\n') + 1
    lazy val col = if (offset == 0) {
      1
    } else {
      offset - input.lastIndexWhere(_ == '\n', offset - 1)
    }

    lazy val sourceLine: String =
      input.substring(offset - (col - 1)).takeWhile(_ != '\n')

    def subInput: String = input.substring(offset)

    def advance(length: Int): Location = copy(offset = offset + length)

    def toError(message: String): ParseError =
      ParseError.Nil.push(this, message)
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
      val as: SGen[String] = SGen { size =>
        Gen.unit("a" * size)
      }

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
              case Left(error) =>
                error match {
                  case ParseError.Nil              => true
                  case ParseError.Cons(head, tail) => head._2 == msg
                }
              case Right(_) => true
            }
        }
    }
  }

  sealed trait JSON {
    def pretty(indent: Int = 0): String
  }

  object JSON {
    case object JNull extends JSON {
      def pretty(indent: Int): String = "null"
    }

    case class JString(get: String) extends JSON {
      def pretty(indent: Int): String = {
        val replacements = Map(
          '"' -> "\\\"",
          '\\' -> "\\\\",
          '\b' -> "\\b",
          '\f' -> "\\f",
          '\n' -> "\\n",
          '\r' -> "\\f",
          '\t' -> "\\t"
        )
        "\"" + get.flatMap { ch =>
          replacements.get(ch).getOrElse(ch.toString)
        } + "\""
      }
    }

    case class JNumber(get: Double) extends JSON {
      def pretty(indent: Int): String = get.toString
    }
    case class JBool(get: Boolean) extends JSON {
      def pretty(indent: Int): String = get.toString
    }

    case class JArray(get: IndexedSeq[JSON]) extends JSON {
      def pretty(indent: Int): String =
        get
          .map { elem =>
            "  " * (indent + 1) + elem.pretty(indent + 1)
          }
          .mkString("[\n", ",\n", s"\n${"  " * indent}]")
    }

    case class JObject(get: Map[String, JSON]) extends JSON {
      def pretty(indent: Int): String =
        get
          .map {
            case key -> value =>
              val prettyKey = JString(key).pretty(0)
              val prettyValue = value.pretty(indent + 1)
              "  " * (indent + 1) + prettyKey + ": " + prettyValue
          }
          .mkString("{\n", ",\n", s"\n${"  " * indent}}")
    }

    def gen: Gen[JSON] = {
      val literal = Gen.weighted(
        Gen.unit(JSON.JNull) -> 1.0,
        Gen.unit(JSON.JBool(false)) -> 1.0,
        Gen.unit(JSON.JBool(true)) -> 1.0,
        Gen.int.map(JSON.JNumber(_)) -> 1.0,
        Gen.string.map(JSON.JString(_)) -> 1.0
      )

      def obj: Gen[JSON] =
        Gen
          .choose(0, 10)
          .flatMap(Gen.listOfN(_, Gen.string ** element))
          .map(x => JSON.JObject(x.toMap))

      def array: Gen[JSON] =
        Gen
          .choose(0, 10)
          .flatMap(Gen.listOfN(_, element))
          .map(x => JSON.JArray(x.toIndexedSeq))

      def element: Gen[JSON] =
        Gen.weighted(
          literal -> 18.0,
          obj -> 1.0,
          array -> 1.0
        )

      Gen.weighted(obj -> 1.0, array -> 1.0)
    }
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

    def mapErrors(f: ParseError => ParseError): ParseResult[A]

    def toEither: Either[Seq[ParseError], A]
  }

  case class Succeeded[A](a: A, length: Int) extends ParseResult[A] {
    def flatMap[B](f: (A, Int) => ParseResult[B]): ParseResult[B] =
      f(a, length)

    def mapErrors(f: ParseError => ParseError): ParseResult[A] = this

    def toEither: Either[Seq[ParseError], A] = Right(a)
  }

  case class FailedUncommitted(errors: Seq[ParseError])
      extends ParseResult[Nothing] {
    def flatMap[B](
        f: (Nothing, Int) => ParseResult[B]
    ): ParseResult[Nothing] = this

    def mapErrors(f: ParseError => ParseError): ParseResult[Nothing] =
      FailedUncommitted(errors.map(f(_)))

    def toEither: Either[Seq[ParseError], Nothing] = Left(errors)
  }

  object FailedUncommitted {
    def apply(error: ParseError): FailedUncommitted = apply(Seq(error))
  }

  case class FailedCommitted(errors: Seq[ParseError])
      extends ParseResult[Nothing] {
    def flatMap[B](
        f: (Nothing, Int) => ParseResult[B]
    ): ParseResult[Nothing] = this

    def mapErrors(f: ParseError => ParseError): ParseResult[Nothing] =
      FailedCommitted(errors.map(f(_)))

    def toEither: Either[Seq[ParseError], Nothing] = Left(errors)
  }

  object FailedCommitted {
    def apply(error: ParseError): FailedCommitted = apply(Seq(error))
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

          val commonPrefix = subInput
            .zip(s)
            .takeWhile {
              case (ch1, ch2) => ch1 == ch2
            }
            .map(_._1)

          if (commonPrefix.length == s.length) {
            Succeeded(s, s.length)
          } else {
            val ch = if (commonPrefix.length >= subInput.length) {
              "<eof>"
            } else {
              subInput.charAt(commonPrefix.length).toString
            }

            val error = location
              .advance(commonPrefix.length)
              .toError(
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
            case FailedUncommitted(errors) =>
              p2.parse(location) match {
                case s: Succeeded[A] => s
                case FailedUncommitted(errors2) =>
                  FailedUncommitted(errors ++ errors2)
                case FailedCommitted(errors2) =>
                  FailedCommitted(errors ++ errors2)
              }
            case res => res
          }

        def slice(location: Location): ParseResult[String] =
          p1.slice(location) match {
            case FailedUncommitted(errors) =>
              p2.slice(location) match {
                case s: Succeeded[String] => s
                case FailedUncommitted(errors2) =>
                  FailedUncommitted(errors ++ errors2)
                case FailedCommitted(errors2) =>
                  FailedCommitted(errors ++ errors2)
              }
            case res => res
          }
      }

    def label[A](name: String)(p: MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p.parse(location).mapErrors(_.label(name))

        def slice(location: Location): ParseResult[String] =
          p.slice(location).mapErrors(_.label(name))
      }

    def scope[A](name: String)(p: MyParser[A]): MyParser[A] =
      new MyParser[A] {
        def parse(location: Location): ParseResult[A] =
          p.parse(location).mapErrors(_.push(location, name))

        def slice(location: Location): ParseResult[String] =
          p.slice(location).mapErrors(_.push(location, name))
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
      runAll(p)(input).swap.map { errors =>
        errors.maxBy { error =>
          error.lastLocation.offset
        }
      }.swap

    def runAll[A](p: MyParser[A])(input: String): Either[Seq[ParseError], A] =
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
            .mapValue(_.reverse)
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

  def jsonLaw[Parser[+_]](p: Parsers[Parser]): Prop = {
    val parser = jsonParser(p)
    forAll(JSON.gen) { json => p.run(parser)(json.pretty()) == Right(json) }
  }
}
