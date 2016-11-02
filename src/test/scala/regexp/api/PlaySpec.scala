package regexp.api

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlaySpec extends FlatSpec with Matchers {

  import Builder._

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
