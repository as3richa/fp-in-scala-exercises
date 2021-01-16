package fp_in_scala_exercises
import ch5.Stream
import ch7.nonblocking.Par
import ch8.{Gen, SGen, Prop, forAll, run}
import _root_.fp_in_scala_exercises.ch3.Tree

object ch10 {
  trait Monoid[A] { self =>
    def op(x: A, y: A): A
    def zero: A

    def dual: Monoid[A] =
      new Monoid[A] {
        def op(x: A, y: A): A = self.op(y, x)
        def zero: A = self.zero
      }
  }

  def monoid[A](z: A)(f: (A, A) => A) =
    new Monoid[A] {
      def op(x: A, y: A): A = f(x, y)
      val zero = z
    }

  val stringConcat: Monoid[String] = monoid("")(_ + _)
  val intAddition: Monoid[Int] = monoid(0)(_ + _)
  def listConcat[A]: Monoid[List[A]] = monoid(List.empty[A])(_ ++ _)
  val intMultiplication: Monoid[Int] = monoid(1)(_ * _)
  val booleanOr: Monoid[Boolean] = monoid(false)(_ || _)
  val booleanAnd: Monoid[Boolean] = monoid(true)(_ && _)
  def option[A]: Monoid[Option[A]] =
    monoid(Option.empty[A])((x, y) => x.orElse(y))
  def endofunction[A]: Monoid[A => A] =
    monoid((a: A) => a)((f, g) => f.compose(g))

  def monoidLaws[A](
      m: Monoid[A],
      gen: Gen[A],
      eq: (A, A) => Boolean = (x: A, y: A) => x == y
  ): Prop = {
    val identity =
      forAll(gen)(a => eq(m.op(a, m.zero), a) && eq(m.op(m.zero, a), a))
        .withTag("identity")

    val associativity =
      forAll(gen ** gen ** gen) {
        case ((x, y), z) => eq(m.op(x, m.op(y, z)), m.op(m.op(x, y), z))
      }.withTag("associativity")

    identity && associativity
  }

  def testMonoidLaws: Unit = {
    val intListGen = Gen.choose(0, 100).flatMap(Gen.listOfN(_, Gen.int))
    val intEndoGen = (Gen.int ** Gen.int).map {
      case (m, b) =>
        (x: Int) => m * x + b
    }
    def linearIntEndoEq(f: Int => Int, g: Int => Int) =
      f(0) == g(0) && f(1) == g(1)

    Seq(
      monoidLaws(stringConcat, Gen.string),
      monoidLaws(intAddition, Gen.int),
      monoidLaws(intMultiplication, Gen.int),
      monoidLaws(listConcat[Int], intListGen),
      monoidLaws(booleanOr, Gen.boolean),
      monoidLaws(booleanAnd, Gen.boolean),
      monoidLaws(option[Int], Gen.option(Gen.int)),
      monoidLaws(endofunction[Int], intEndoGen, linearIntEndoEq(_, _))
    ).foreach(run(_))
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op(_, _))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endofunction[B])(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endofunction[B].dual)(a => f(_, a))(z)

  def testFolds: Unit = {
    run(forAll(SGen.listOf(Gen.int)) { list =>
      foldMap(list, intAddition)(x => x) == foldMap2(list, intAddition)(x => x)
    })

    run(forAll(SGen.listOf(Gen.int)) { list =>
      foldLeft(list)(0)(_ + _) == list.sum &&
      foldRight(list)(0)(_ + _) == list.sum &&
      foldLeft(list)(List.empty[Int])((l, a) => a :: l) == list.reverse &&
      foldRight(list)(List.empty[Int])(_ :: _) == list
    })
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length <= 1) {
      as.headOption.map(f(_)).getOrElse(m.zero)
    } else {
      val (left, right) = as.splitAt(as.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    monoid(Par.unit(m.zero))(Par.map2(_, _)((x, y) => m.op(x, y)))

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val p = par(m)

    def parFoldMap0(as: IndexedSeq[A]): Par[B] =
      if (as.length <= 1) {
        as.headOption.map(a => Par.unit(f(a))).getOrElse(p.zero)
      } else {
        val (left, right) = as.splitAt(as.length / 2)
        p.op(Par.fork { parFoldMap0(left) }, Par.fork { parFoldMap0(right) })
      }

    parFoldMap0(as)
  }

  def isSorted[A](xs: IndexedSeq[A])(implicit ord: Ordering[A]): Boolean = {
    sealed trait OrderingState
    case object Empty extends OrderingState
    case class Ordered(min: A, max: A) extends OrderingState
    case object Unordered extends OrderingState

    val m = monoid[OrderingState](Empty) {
      case (Empty, right)     => right
      case (left, Empty)      => left
      case (Unordered, right) => Unordered
      case (left, Unordered)  => Unordered
      case (Ordered(leftMin, leftMax), Ordered(rightMin, rightMax)) =>
        if (ord.lteq(leftMax, rightMin)) {
          Ordered(leftMin, rightMax)
        } else {
          Unordered
        }
    }

    foldMapV(xs, m)(x => Ordered(x, x)) match {
      case Empty         => true
      case Ordered(_, _) => true
      case Unordered     => false
    }
  }

  def testIsSorted: Unit = {
    val genSorted = SGen.listOf(Gen.int).map(_.toIndexedSeq.sorted)

    run(forAll(SGen.listOf(Gen.int).map(_.toIndexedSeq)) { seq =>
      isSorted(seq) == (seq == seq.sorted)
    })

    run(forAll(genSorted) { seq => isSorted(seq) })
  }

  sealed trait WC {
    def count: Int
  }

  object WC {
    def apply(ch: Char): WC =
      if (ch.isWhitespace) {
        Part("", 0, "")
      } else {
        Stub(ch.toString)
      }
  }
  case class Stub(chars: String) extends WC {
    def count: Int = if (chars.isEmpty) 0 else 1
  }

  case class Part(lstub: String, words: Int, rstub: String) extends WC {
    def count: Int =
      words + (if (lstub.isEmpty) 0 else 1) + (if (rstub.isEmpty) 0 else 1)
  }

  val wcMonoid = monoid[WC](Stub("")) {
    case (Stub(left), Stub(right)) => Stub(left + right)
    case (Stub(left), Part(lstub, words, rstub)) =>
      Part(left + lstub, words, rstub)
    case (Part(lstub, words, rstub), Stub(right)) =>
      Part(lstub, words, rstub + right)
    case (Part(lLstub, lWords, lRstub), Part(rLstub, rWords, rRstub)) => {
      val wordsFormed = if (!lRstub.isEmpty || !rLstub.isEmpty) {
        1
      } else {
        0
      }
      Part(lLstub, lWords + wordsFormed + rWords, rRstub)
    }
  }

  def wc(s: String): WC =
    foldMapV(s.toIndexedSeq, wcMonoid)(WC(_))

  def wordCount(s: String): Int = wc(s).count

  def testWc: Unit = {
    val word = Gen
      .choose(1, 20)
      .flatMap(Gen.listOfN(_, Gen.choose('a'.toInt, 'z'.toInt).map(_.toChar)))
      .map(_.mkString)

    val whitespace = Gen
      .choose(1, 20)
      .flatMap(
        Gen.listOfN(
          _,
          Gen.weighted(
            Gen.unit(' ') -> 1.0,
            Gen.unit('\t') -> 1.0,
            Gen.unit('\n') -> 1.0
          )
        )
      )
      .map(_.mkString)

    val words =
      (word ** whitespace)
        .map { case (word, space) => word + space }
        .listOfN(Gen.choose(0, 100))
        .map(_.mkString)

    run(forAll(words) { s =>
      wordCount(s) == "[a-z]+".r.findAllIn(s).length
    })

    run(monoidHomophorismLaw(stringConcat, wcMonoid)(wc(_))(words))
  }

  def monoidHomophorismLaw[A, B](ma: Monoid[A], mb: Monoid[B])(
      f: A => B
  )(a: Gen[A]): Prop = {
    forAll(a ** a) {
      case (x, y) => f(ma.op(x, y)) == mb.op(f(x), f(y))
    }
  }

  def testLengthLaw: Unit = {
    val gen = Gen.int.listOfN(Gen.choose(0, 100))
    run(
      monoidHomophorismLaw(listConcat[Int], intAddition)(_.length)(gen)
    )
  }

  trait Foldable[F[_]] {
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as)(m.zero) {
        case (b, a) => m.op(b, f(a))
      }

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op(_, _))

    def toList[A, B](as: F[A]): List[A] =
      foldRight(as)(List.empty[A])(_ :: _)
  }

  val listFoldable: Foldable[List] =
    new Foldable[List] {
      def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f(_, _))

      def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f(_, _))
    }

  val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f(_, _))

    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f(_, _))

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
      foldMapV(as, m)(f(_))
  }

  val streamFoldable: Foldable[Stream] = new Foldable[Stream] {
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f(_, _))

    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f(_, _))
  }

  val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
      Tree.fold(as)(f)(m.op(_, _))

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => f(_, a))(endofunction[B].dual)(z)

    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(a => f(a, _))(endofunction[B])(z)
  }

  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.map(f(z, _)).getOrElse(z)

    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.map(f(_, z)).getOrElse(z)
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    monoid((a.zero, b.zero)) {
      case ((ax, bx), (ay, by)) => (a.op(ax, ay), b.op(bx, by))
    }

  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] =
    monoid(Map.empty[K, V]) { (x, y) =>
      x.map {
        case (k, v) =>
          k -> m.op(v, y.get(k).getOrElse(m.zero))
      } ++ y.filter {
        case (k, _) => !x.contains(k)
      }
    }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] =
    monoid((a: A) => m.zero)((f, g) => (a: A) => m.op(f(a), g(a)))

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    indexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(
      mapMergeMonoid(intAddition)
    )

  def testBag: Unit = {
    val gen = SGen.listOf(Gen.choose(0, 100)).map(_.toIndexedSeq)
    run(forAll(gen) { seq =>
      bag(seq) == seq.foldLeft(Map.empty[Int, Int]) {
        case (map, x) => map.updated(x, map.get(x).getOrElse(0) + 1)
      }
    })
  }

  def mean(xs: List[Int]): Double =
    listFoldable.foldMap(xs)((_, 1))(
      productMonoid(intAddition, intAddition)
    ) match {
      case (sum, length) => sum.toDouble / length
    }
}
