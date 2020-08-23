package fp_in_scala_exercises

object ch4 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](f: => B): B
    def orElse[B >: A](f: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case class Some[A](value: A) extends Option[A] { self =>
    override def map[B](f: A => B): Option[B] = Some(f(value))
    override def flatMap[B](f: A => Option[B]): Option[B] = f(value)
    override def getOrElse[B >: A](f: => B): B = value
    override def orElse[B >: A](f: => Option[B]): Option[B] = self
    override def filter(f: A => Boolean) =
      if (f(value)) {
        self
      } else {
        None
      }
  }

  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None
    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    override def getOrElse[B](f: => B): B = f
    override def orElse[B](f: => Option[B]): Option[B] = f
    override def filter(f: Nothing => Boolean): Option[Nothing] = None
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.length)
    }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(xsMean => mean(xs.map(x => Math.pow(x - xsMean, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  val abs0: Option[Double] => Option[Double] = lift(math.abs(_))

  def Try[A](f: => A): Option[A] =
    try {
      Some(f)
    } catch {
      case _: Throwable => None
    }

  def toIntOpt(str: String): Option[Int] = Try(str.toInt)

  def insuranceRateQuote(age: Int, tickets: Int): Double = 1337.0

  def parseInsuranceRateQuote0(age: String, tickets: String): Option[Double] = {
    toIntOpt(age).flatMap(age =>
      toIntOpt(tickets).map(tickets => insuranceRateQuote(age, tickets))
    )
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a => b.map(b => f(a, b)))
  }

  def parseInsuranceRateQuote(age: String, tickets: String): Option[Double] =
    map2(toIntOpt(age), toIntOpt(tickets))(insuranceRateQuote)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty[A])) {
      case (aOpt, listOpt) => map2(aOpt, listOpt)((a, list) => a :: list)
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)) {
      case (a, listOpt) => map2(f(a), listOpt)(_ :: _)
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def map22[A, B, C](aOpt: Option[A], bOpt: Option[B])(
      f: (A, B) => C
  ): Option[C] =
    for {
      a <- aOpt
      b <- bOpt
    } yield {
      f(a, b)
    }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, AA >: A](f: => Either[EE, AA]): Either[EE, AA]
    def map2[EE >: E, B, C](
        other: Either[EE, B]
    )(f: (A, B) => C): Either[EE, C] =
      flatMap(a => other.map(b => f(a, b)))
  }

  case class Left[E](value: E) extends Either[E, Nothing] { self =>
    override def map[B](f: Nothing => B): Either[E, B] = self
    override def flatMap[EE >: E, B](
        f: Nothing => Either[EE, B]
    ): Either[EE, B] = self
    override def orElse[EE >: E, AA >: Nothing](f: => Either[EE, AA]) = f
  }

  case class Right[A](value: A) extends Either[Nothing, A] { self =>
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    override def flatMap[EE, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    override def orElse[EE, AA >: A](f: => Either[EE, AA]): Either[EE, AA] =
      self
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      es.foldRight[Either[E, List[A]]](Right(Nil)) {
        case (value, list) => value.map2(list)(_ :: _)
      }

    def traverse[A, E, B](
        es: List[A]
    )(f: A => Either[E, B]): Either[E, List[B]] =
      es.foldRight[Either[E, List[B]]](Right(Nil)) {
        case (a, list) => f(a).map2(list)(_ :: _)
      }

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(identity)
  }

  def mean2(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) {
      Left("aw heck")
    } else {
      Right(xs.sum / xs.length)
    }
}
