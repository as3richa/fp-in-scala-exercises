package fp_in_scala_exercises
import ch8.{Gen, Prop, forAll}
import scala.util.matching.Regex

object ch9 {
  trait Parsers[ParseError, Parser[+_]] { self =>
    implicit def string(s: String): Parser[String]

    implicit def char(c: Char): Parser[Char] =
      string(c.toString).map(_.charAt(0))

    def charClass(f: Char => Boolean): Parser[Char]

    implicit def regex(r: Regex): Parser[String]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    implicit def asStringParser[A](a: A)(implicit
        f: A => Parser[String]
    ): ParserOps[String] = ParserOps(f(a))

    def succeed[A](a: A): Parser[A]

    def fail[A](error: ParseError): Parser[A]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      flatMap(p) { a => succeed(f(a)) }

    def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C) =
      flatMap(p1) { a => map(p2)(f(a, _)) }

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def fold[A, B](p: Parser[A])(z: B)(f: (B, A) => B): Parser[B] =
      or(flatMap(p) { a => fold(p)(f(z, a))(f) }, succeed(z))

    def foldRight[A, B](p: Parser[A])(z: B)(f: (A, B) => B): Parser[B] =
      or(map2(p, foldRight(p)(z)(f))(f), succeed(z))

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      map2(p1, p2)((_, _))

    def optional[A](p: Parser[A]): Parser[Option[A]]

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

    val number: Parser[Double] = {
      val pNonzeroDigit = charClass('1'.to('9').contains(_))
      val pDigits = fold(charClass('0'.to('9').contains(_)))(())((_, _) => ())

      val pIntegerPart =
        optional(char('-')) ** (char('0') | (pNonzeroDigit ** pDigits))

      val pFractionalPart = char('.') ** many1(pDigits)

      val pExponent =
        (char('e') | char('E')) ** optional(char('+') | char('-')) ** pDigits

      (pIntegerPart ** optional(pFractionalPart) ** optional(pExponent)).slice
        .map(_.toDouble)
    }

    val discardWhitespace: Parser[Unit] = {
      def isWhitespace(char: Char): Boolean =
        char match {
          case ' ' | '\t' | '\r' | '\n' => true
          case _                        => false
        }
      fold(charClass(isWhitespace(_)))(())((_, _) => ())
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
        (char('u') ** listOfN(4, hexDigit).slice).map {
          case (_, codepoint) => codepoint.toInt.toChar
        }

      val escape = (char('\\') ** (simpleEscapeCode | uEscapeCode)).map(_._2)

      val pRegularChar =
        charClass(char => !char.isControl && char != '"' && char != '\\')

      fold(pRegularChar | escape)(new StringBuilder)(_.addOne(_))
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

  def jsonParser[ParseError, Parser[+_]](
      p: Parsers[ParseError, Parser]
  ): Parser[JSON] = {
    import p._
    import JSON._

    def literal: Parser[JSON] =
      Seq(
        string("null").map(_ => JNull),
        number.map(s => JNumber(s.toDouble)),
        escapedString.map(JString(_)),
        string("true").map(_ => JBool(true)),
        string("false").map(_ => JBool(false))
      ).reduce(_ | _)

    def array: Parser[JSON] =
      bookend('[', ']') {
        element.manySep(",").map { elems =>
          JArray(elems.toIndexedSeq)
        }
      }

    def obj: Parser[JSON] = {
      val key = (discardWhitespace >* escapedString <* discardWhitespace)
      bookend('{', '}') {
        (key ** (char(':') >* element))
          .manySep(",")
          .map(pairs => JObject(pairs.toMap))
      }
    }

    def element: Parser[JSON] =
      (discardWhitespace >* (literal | array | obj) <* discardWhitespace)

    discardWhitespace >* array | obj <* discardWhitespace
  }
}
