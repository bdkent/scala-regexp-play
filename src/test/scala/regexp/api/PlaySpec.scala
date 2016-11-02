package regexp.api

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import regexp.api.semirings._

class PlaySpec extends FlatSpec with Matchers {

  import Builder._

  "Matcher" should "pass Act I, Scene I" in {

    implicit object BooleanInstance extends Boolean_Semiring

    val nocs = repetition(alternative(symbolIs('a'), symbolIs('b')))

    val onec = sequence(nocs, symbolIs('c'))

    val evencs = sequence(repetition(sequence(onec, onec)), nocs)

    StringMatcher.accept(evencs, "acc") should be(true)
  }

  "Matcher" should "pass Act I, Scene II" in {

    implicit object IntInstance extends Int_Semiring

    val as = alternative(symbolZero('a'), repetition(symbolZero('a')))

    val bs = alternative(symbolZero('b'), repetition(symbolZero('b')))

    StringMatcher.accept(as, "a") should be(2)

    StringMatcher.accept(sequence(as, bs), "ab") should be(4)

    StringMatcher.accept(repetition(Builder.empty()), "") should be(1)

  }

  "Matcher" should "pass Act II, Scene II: Leftmost" in {

    implicit object LeftmostInstance extends Leftmost_IndexedSemiring

    val a = symbol('a')

    val ab = repetition(alternative(a, symbol('b')))

    val aaba = sequence(sequence(a, ab), a)

    StringMatcher.submatch(aaba, "ab") should be(NoLeft)

    StringMatcher.submatch(aaba, "aa") should be(SomeLeftmost(SomeStart(0)))

    StringMatcher.submatch(aaba, "bababa") should be(SomeLeftmost(SomeStart(1)))
  }

  "Matcher" should "pass Act II, Scene II: LeftLong" in {

    implicit object LeftLongInstance extends LeftLong_IndexedSemiring

    val a = symbol('a')

    val ab = repetition(alternative(a, symbol('b')))

    val aaba = sequence(sequence(a, ab), a)

    StringMatcher.submatch(aaba, "ab") should be(NoLeftLong)

    StringMatcher.submatch(aaba, "aa") should be(SomeLeftLong(SomeRange(0, 1)))

    StringMatcher.submatch(aaba, "bababa") should be(SomeLeftLong(SomeRange(1, 5)))
  }

  "Matcher" should "pass Act II, Scene III" in {
    implicit object BooleanInstance extends Boolean_Semiring
    val a = symbol({ (c: Char) => c == 'a' })

    def sean(n: Int, r: Lazy[State[Char, Boolean]]) = {
      List.fill(n)(r).reduce(sequence(_, _))
    }

    def re[C](n: Int) = sequence(sean(n, alternative(a, Builder.empty())), sean(n, a))

    Matcher.run(re(5), List.fill(5)('a')) should be(true)
  }

  "Matcher" should "pass Act III, Scene I" in {
    implicit object BooleanInstance extends Boolean_Semiring

    val a = symbol({ (c: Char) => c == 'a' })

    val b = symbol({ (c: Char) => c == 'b' })

    lazy val anbn: Lazy[State[Char, Boolean]] = alternative(Builder.empty(), sequence(a, sequence(anbn, b)))

    StringMatcher.run(anbn, "") should be(true)
  }

  "Matcher" should "pass Act III, Scene II" in {

    implicit object BooleanInstance extends Boolean_Semiring

    val a = symbol({ (c: Char) => c == 'a' })

    val b = symbol({ (c: Char) => c == 'b' })

    lazy val anbn: Lazy[State[Char, Boolean]] = alternative(Builder.empty(), sequence(a, sequence(anbn, b)))

    StringMatcher.run(anbn, "") should be(true)
    StringMatcher.run(anbn, "ab") should be(true)
    StringMatcher.run(anbn, "aabb") should be(true)
    StringMatcher.run(anbn, "aabbb") should be(false)

    def bs(n: Int) = List.fill(n)(symbol({ (c: Char) => c == 'b' }))

    def cs(n: Int) = List.fill(n)(symbol({ (c: Char) => c == 'c' }))

    def bcs(n: Int) = (bs(n) ++ cs(n)).reduce(sequence(_, _))

    def abc(n: Int): Lazy[State[Char, Boolean]] = sequence(a, alternative(bcs(n), abc(n + 1)))

    val anbncn = alternative(Builder.empty(), abc(1))

    StringMatcher.run(anbncn, "") should be(true)
    StringMatcher.run(anbncn, "abc") should be(true)
    StringMatcher.run(anbncn, "aabbcc") should be(true)
    StringMatcher.run(anbncn, "aabbbcc") should be(false)
  }
}
