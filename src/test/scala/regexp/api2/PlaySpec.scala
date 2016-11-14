package regexp.api2

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import regexp.api.Semiring
import regexp.api.IndexedSemiring
import regexp.api.semirings._

class PlaySpec extends FlatSpec with Matchers {

  def symbolIndex[C, S](c: C)(implicit semi: IndexedSemiring[S]): RegW[S, (C, Int)] = {
    def f(t: (C, Int)): S = {
      t match {
        case (x, pos) if x == c => semi.index(pos)
        case _                  => semi.zero
      }
    }
    Regex.symW(Nil, f _)
  }

  "Matcher" should "pass Act I, Scene I" in {

    implicit object BooleanInstance extends Boolean_Semiring
    implicit object BooleanWeightInstance extends Boolean_Weight[Char]

    val nocs = Regex.rep(Regex.alt(Regex.char('a'), Regex.char('b')))

    val onec = Regex.seq_(nocs, Regex.char('c'))

    val evencs = Regex.seq_(Regex.rep(Regex.seq_(onec, onec)), nocs)

    Matcher.acceptFull(evencs, "acc".toList) should be(true)
  }

  "Matcher" should "pass Act I, Scene II" in {

    implicit object IntInstance extends Int_Semiring
    implicit object IntWeightInstance extends Int_Weight[Char]

    def symbolZero[S](c: Char)(implicit semi: Semiring[S]): RegW[S, Char] = {
      def f(x: Char): S = {
        if (x == c) semi.one else semi.zero
      }
      Regex.symW(Nil, f _)
    }

    val as = Regex.alt(symbolZero('a'), Regex.rep(symbolZero('a')))

    val bs = Regex.alt(symbolZero('b'), Regex.rep(symbolZero('b')))

    Matcher.acceptFull(as, "a".toList) should be(2)

    Matcher.acceptFull(Regex.seq_(as, bs), "ab".toList) should be(4)

    Matcher.acceptFull(Regex.rep(Regex.eps()), "".toList) should be(1)

  }

  "Matcher" should "pass Act II, Scene II: Leftmost" in {

    implicit object LeftmostInstance extends Leftmost_IndexedSemiring
    implicit val LeftmostWeightInstance = new Leftmost_Weight2[Char]()(LeftmostInstance)

    val a = symbolIndex('a')

    val ab = Regex.rep(Regex.alt(a, symbolIndex('b')))

    val aaba = Regex.seq_(Regex.seq_(a, ab), a)

    Matcher.partialMatch(aaba, "ab".toList.zipWithIndex) should be(NoLeft)

    Matcher.partialMatch(aaba, "aa".toList.zipWithIndex) should be(SomeLeftmost(SomeStart(0)))

    Matcher.partialMatch(aaba, "bababa".toList.zipWithIndex) should be(SomeLeftmost(SomeStart(1)))
  }

  "Matcher" should "pass Act II, Scene II: LeftLong" in {

    implicit object LeftLongInstance extends LeftLong_IndexedSemiring
    implicit val LeftmostWeightInstance = new LeftLong_Weight2[Char]()(LeftLongInstance)

    val a = symbolIndex('a')

    val ab = Regex.rep(Regex.alt(a, symbolIndex('b')))

    val aaba = Regex.seq_(Regex.seq_(a, ab), a)

    Matcher.partialMatch(aaba, "ab".toList.zipWithIndex) should be(NoLeftLong)

    Matcher.partialMatch(aaba, "aa".toList.zipWithIndex) should be(SomeLeftLong(SomeRange(0, 1)))

    Matcher.partialMatch(aaba, "bababa".toList.zipWithIndex) should be(SomeLeftLong(SomeRange(1, 5)))
  }

  "Matcher" should "pass Act II, Scene III" in {

    implicit object BooleanInstance extends Boolean_Semiring
    implicit object BooleanWeightInstance extends Boolean_Weight[Char]

    val a = Regex.char('a')

    def seqn(n: Int, r: RegW[Boolean, Char]) = {
      List.fill(n)(r).reduce(Regex.seq_(_, _))
    }

    def re[C](n: Int) = Regex.seq_(seqn(n, Regex.alt(a, Regex.eps())), seqn(n, a))

    val n = 10

    Matcher.fullMatch(re(n), List.fill(n)('a')) should be(true)
  }

  "Matcher" should "pass Act III, Scene I" in {
    implicit object BooleanInstance extends Boolean_Semiring
    implicit object BooleanWeightInstance extends Boolean_Weight[Char]

    val a = Regex.char('a')

    val b = Regex.char('b')

    lazy val anbn: RegW[Boolean, Char] = Regex.alt(Regex.eps(), Regex.seq_(a, Regex.seq_(anbn, b)))

    Matcher.fullMatch(anbn, "".toList) should be(true)
  }

  "Matcher" should "pass Act III, Scene II" in {

    implicit object BooleanInstance extends Boolean_Semiring
    implicit object BooleanWeightInstance extends Boolean_Weight[Char]

    val a = Regex.char('a')

    val b = Regex.char('b')

    def anbn(): RegW[Boolean, Char] = Regex.alt(Regex.eps(), Regex.seq_(a, Regex.seq_(anbn(), b)))

    Matcher.fullMatch(anbn, "".toList) should be(true)
    Matcher.fullMatch(anbn, "ab".toList) should be(true)
    Matcher.fullMatch(anbn, "aabb".toList) should be(true)
    Matcher.fullMatch(anbn, "aabbb".toList) should be(false)

    def bs(n: Int) = List.fill(n)(Regex.char('b'))

    def cs(n: Int) = List.fill(n)(Regex.char('c'))

    def bcs(n: Int) = (bs(n) ++ cs(n)).reduce(Regex.seq_(_, _))

    def abc(n: Int): RegW[Boolean, Char] = Regex.seq_(a, Regex.alt(bcs(n), abc(n + 1)))

    val anbncn = Regex.alt(Regex.eps(), abc(1))

    Matcher.fullMatch(anbncn, "".toList) should be(true)
    Matcher.fullMatch(anbncn, "abc".toList) should be(true)
    Matcher.fullMatch(anbncn, "aabbcc".toList) should be(true)
    Matcher.fullMatch(anbncn, "aabbbcc".toList) should be(false)
  }
}
