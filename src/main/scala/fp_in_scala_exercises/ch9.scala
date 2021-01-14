package fp_in_scala_exercises
import ch8.{Gen, SGen, Prop, forAll}
import scala.util.matching.Regex
import _root_.fp_in_scala_exercises.ch8.SGen

object ch9 {
  case class ParseError(stack: List[(Location, String)])

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset).count(_ == '\n') + 1
    lazy val col = if (offset == 0) {
      1
    } else {
      offset - input.lastIndexWhere(_ == '\n', offset - 1)
    }
  }

  trait Parsers[Parser[+_]] { self =>
    implicit def string(s: String): Parser[String]

    def charClass(f: Char => Boolean): Parser[Char]

    implicit def regex(r: Regex): Parser[String]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    implicit def asStringParser[A](a: A)(implicit
        f: A => Parser[String]
    ): ParserOps[String] = ParserOps(f(a))

    def succeed[A](a: A): Parser[A]

    def fail[A](message: String): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def label[A](name: String)(p: Parser[A]): Parser[A]

    def scope[A](name: String)(p: Parser[A]): Parser[A]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def char(ch: Char): Parser[Char] =
      string(ch.toString).map(_.charAt(0))

    def chars(chs: Char*): Parser[Char] = charClass(chs.contains(_))

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      flatMap(p) { a => succeed(f(a)) }

    def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C) =
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
      fold(p)(List.empty[A]) { (as, a) => a :: as }

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map(product(p, many(p))) {
        case (a1, as) => a1 :: as
      }

    def bookend[A, B, C](left: Parser[A], right: Parser[B])(
        p: Parser[C]
    ): Parser[C] =
      product(product(left, p), right).map {
        case ((_, c), _) => c
      }

    def manySep[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
      succeed(Nil) | map(product(many(product(p, sep).map(_._1)), p)) {
        case (as, a) => a :: as
      }

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
      fold(charClass(isWhitespace(_)))(())((_, _) => ())
    }

    val number: Parser[Double] = {
      val nonzeroDigit = charClass('1'.to('9').contains(_))

      val discardDigits =
        fold(charClass('0'.to('9').contains(_)))(())((_, _) => ())

      val integer =
        (optional(char('-')) ** (char('0') | (nonzeroDigit ** discardDigits)))
          .label("integer part")

      val fractionalPart =
        (char('.') ** many1(discardDigits)).label("fractional part")

      val exponent =
        ((chars('e', 'E')) ** optional(
          chars('+', '-')
        ) ** discardDigits).label("exponent")

      (integer ** optional(fractionalPart) ** optional(exponent)).slice
        .map(_.toDouble)
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

      val hexDigit = charClass { ch =>
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
        charClass(char => !char.isControl && char != '"' && char != '\\')

      val unexpectedControlChar =
        fail("unexpected control character")

      fold(regularChar | escape | unexpectedControlChar)(new StringBuilder)(
        _.addOne(_)
      )
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
      bookend('[', ']') {
        element.manySep(",").map { elems =>
          JArray(elems.toIndexedSeq)
        }
      }.scope("array")

    def obj: Parser[JSON] = {
      val key = (discardWhitespace >* escapedString <* discardWhitespace)
      bookend('{', '}') {
        (key ** (char(':') >* element))
          .manySep(",")
          .map(pairs => JObject(pairs.toMap))
      }.scope("object")
    }

    def element: Parser[JSON] =
      (discardWhitespace >* (literal | array | obj) <* discardWhitespace)

    discardWhitespace >* array | obj <* discardWhitespace
  }

  sealed trait MyParser[+A] {}

  object MyParser extends Parsers[MyParser] {
    implicit def string(s: String): MyParser[String] = ???

    def charClass(f: Char => Boolean): MyParser[Char] = ???

    implicit def regex(r: Regex): MyParser[String] = ???

    def succeed[A](a: A): MyParser[A] = ???

    def fail[A](message: String): MyParser[A] = ???

    def attempt[A](p: MyParser[A]): MyParser[A] = ???

    def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = ???

    def label[A](name: String)(p: MyParser[A]): MyParser[A] = ???

    def scope[A](name: String)(p: MyParser[A]): MyParser[A] = ???

    def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???

    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = ???

    def slice[A](p: MyParser[A]): MyParser[String] = ???
  }
}
