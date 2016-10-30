package regexp.play

import regexp.play.HaskellBridge._
import regexp.play.Semirings._

// NOTE: literal translation of Haskell code that won't actually work at all because Scala evaluates eagerly!!

trait Act3_Scene2_Eager {

  // added a boolean field active to the data type REGw
  case class REGw[+C, +S](active: Boolean = false, emptyw: S, finalw: S, regw: REw[C, S])
  def active[C, S](r: REGw[C, S]): Boolean = r.active
  def emptyw[C, S](r: REGw[C, S]): S = r.emptyw
  def finalw[C, S](r: REGw[C, S]): S = r.finalw
  def regw[C, S](r: REGw[C, S]): REw[C, S] = r.regw

  sealed trait REw[+C, +S]
  case class EPSw[+C, +S]() extends REw[C, S]
  case class SYMw[C, +S](x0: (C => S)) extends REw[C, S]
  case class ALTw[+C, +S](x0: REGw[C, S], x1: REGw[C, S]) extends REw[C, S]
  case class SEQw[+C, +S](x0: REGw[C, S], x1: REGw[C, S]) extends REw[C, S]
  case class REPw[+C, +S](x0: REGw[C, S]) extends REw[C, S]

  def epsw[C, S]()(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = one,
      finalw = zero,
      regw = EPSw())
  }

  def symw[C, S](f: (C => S))(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = zero,
      finalw = zero,
      regw = SYMw(f))
  }

  def altw[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    REGw(
      emptyw = emptyw(p) + emptyw(q),
      finalw = finalw(p) + finalw(q),
      regw = ALTw(p, q))
  }

  def alt[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    altw(p, q).copy(active = false, finalw = zero)
  }

  // seqw :: Semiring s ⇒ REGw c s → REGw c s → REGw c s
  // seqw p q =
  //   REGw { active = active p ∨ active q,
  //          emptyw = emptyw p ⊗ emptyw q,
  //          finalw = finala p ⊗ emptyw q ⊕ finala q,
  //          regw = SEQw p q }
  def seqw[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    REGw(
      active = active(p) ∨ active(q),
      emptyw = emptyw(p) * emptyw(q),
      finalw = (finala(p) * emptyw(q)) + finala(q),
      regw = SEQw(p, q))
  }

  // seq p q = REGw { active = False,
  //                  emptyw = emptyw p ⊗ emptyw q,
  //                  finalw = zero,
  //                  regw = SEQw p q }
  def seq[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    seqw(p, q).copy(active = false, finalw = zero)
  }

  def repw[C, S](r: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = one,
      finalw = finalw(r),
      regw = REPw(r))
  }

  def rep[C, S](r: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    repw(r).copy(active = false, finalw = zero)
  }

  // finala ::Semiring s ⇒ REGw c s → s
  // finala r = if active r then finalw r else zero
  def finala[C, S](r: REGw[C, S])(implicit semi: Semiring[S]): S = {
    import semi._
    if (active(r)) finalw(r) else zero
  }

  def matchw[C, S](r: REGw[C, S], xs: List[C])(implicit semi: Semiring[S]): S = {
    import semi._
    xs match {
      case Nil => emptyw(r)
      case c :: cs =>
        def f(r1: REGw[C, S], c1: C): REGw[C, S] = shiftw(zero, r1, c1)
        finalw(foldl(f, shiftw(one, r, c), cs))
    }
  }

  // shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
  // shiftw ...
  // shiftw m (SYMw f)  c =
  //   let fin = m ⊗ f c
  //   in (symw f) {active = fin = ̸= zero, finalw = fin}
  def stepw[C, S](m: S, r: REw[C, S], c: C)(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    r match {
      case EPSw() => epsw()
      case SYMw(f) => {
        lazy val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
        lazy val fin = m * f1(c)
        symw(f).copy(finalw = fin, active = fin != zero)
      }
      case ALTw(p, q) => altw(shiftw(m, p, c), shiftw(m, q, c))
      case SEQw(p, q) => seqw(shiftw(m, p, c), shiftw(m * emptyw(p) + finalw(p), q, c))
      case REPw(r)    => repw(shiftw(m + finalw(r), r, c))
    }
  }

  // shiftw :: (Eq s, Semiring s) ⇒ s → REGw c s → c → REGw c s
  // shiftw m r c | active r ∨ m =/= zero = stepw m (regw r) c
  //              | otherwise            = r
  def shiftw[C, S](m: S, r: REGw[C, S], c: C)(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    if (active(r) ∨ (m != zero)) {
      stepw(m, regw(r), c)
    } else {
      r
    }
  }

  // submatchw :: Semiring s ⇒ REGw (Int, c) s → [c] → s
  // submatchw r s =
  //   matchw (seqw arb (seqw r arb)) (zip [0..] s)
  //   where arb = repw (symw (λ_ → one))
  def submatchw[C, S](r: REGw[(Int, C), S], s: List[C])(implicit semi: Semiring[S]): S = {
    import semi._
    def f(x: (Int, C)) = one
    lazy val arb = repw(symw(f))
    matchw(seqw(arb, seqw(r, arb)), zipWithIndex(s))
  }

  // class Semiring s ⇒ Semiringi s where
  //   index :: Int → s
  trait Semiringi[S] extends Semiring[S] {
    def index(i: Int): S
  }

  // symi :: Semiringi s ⇒ Char → REGw (Int, Char) s
  // symi c = symw weight
  //   where weight (pos, x) | x == c    = index pos
  //                         | otherwise = zero
  def symi[S](c: Char)(implicit semi: Semiringi[S]): REGw[(Int, Char), S] = {
    import semi._
    def weight(t: (Int, Char)): S = {
      t match {
        case (pos, x) if x == c => index(pos)
        case (pos, x)           => zero
      }
    }
    symw(weight)
  }

  // data Leftmost = NoLeft | Leftmost Start
  sealed trait Leftmost
  case object NoLeft extends Leftmost
  case class Leftmost_(s: Start) extends Leftmost

  // data Start = NoStart | Start Int
  sealed trait Start
  case object NoStart extends Start
  case class Start_(i: Int) extends Start

  // instance Semiring Leftmost where
  // zero = NoLeft
  // one = Leftmost NoStart
  trait Leftmost_Semiring extends Semiring[Leftmost] {
    override val zero: Leftmost = NoLeft
    override val one: Leftmost = Leftmost_(NoStart)
    // NoLeft     ⊕ x          = x
    // x          ⊕ NoLeft     = x
    // Leftmost x ⊕ Leftmost y = Leftmost (leftmost x y)
    //   where leftmost NoStart NoStart = NoStart
    //         leftmost NoStart (Start i) = Start i
    //         leftmost (Start i) NoStart = Start i
    //         leftmost (Start i) (Start j) = Start (min i j)
    override def add(a: Leftmost, b: Leftmost): Leftmost = {
      (a, b) match {
        case (NoLeft, x) => x
        case (x, NoLeft) => x
        case (Leftmost_(x), Leftmost_(y)) => {
          def leftmost(s1: Start, s2: Start): Start = {
            (s1, s2) match {
              case (NoStart, NoStart)     => NoStart
              case (NoStart, Start_(i))   => Start_(i)
              case (Start_(i), NoStart)   => Start_(i)
              case (Start_(i), Start_(j)) => Start_(min(i, j))
            }
          }
          Leftmost_(leftmost(x, y))
        }
      }
    }
    // NoLeft     ⊗ _          = NoLeft
    // _          ⊗ NoLeft     = NoLeft
    // Leftmost x ⊗ Leftmost y = Leftmost (start x y)
    //   where start NoStart s = s
    //         start s       _ = s
    override def multiply(a: Leftmost, b: Leftmost): Leftmost = {
      (a, b) match {
        case (NoLeft, _) => NoLeft
        case (_, NoLeft) => NoLeft
        case (Leftmost_(x), Leftmost_(y)) => {
          def start(s1: Start, s2: Start): Start = {
            (s1, s2) match {
              case (NoStart, s) => s
              case (s, _)       => s
            }
          }
          Leftmost_(start(x, y))
        }
      }
    }
  }

  // instance Semiringi Leftmost where
  //   index = Leftmost · Start
  trait Leftmost_Semiringi extends Semiringi[Leftmost] with Leftmost_Semiring {
    override def index(i: Int): Leftmost = {
      (Leftmost_.apply _).compose(Start_.apply)(i)
    }
  }

  // data LeftLong = NoLeftLong | LeftLong Range
  sealed trait LeftLong
  case object NoLeftLong extends LeftLong
  case class LeftLong_(r: Range) extends LeftLong

  // data Range = NoRange | Range Int Int
  sealed trait Range
  case object NoRange extends Range
  case class Range_(x: Int, y: Int) extends Range

  trait LeftLong_Semiring extends Semiring[LeftLong] {
    override val zero: LeftLong = NoLeftLong
    override val one: LeftLong = LeftLong_(NoRange)

    // LeftLong x ⊕ LeftLong y = LeftLong (leftlong x y)
    // where leftlong ...
    //       leftlong (Range i j) (Range k l)
    //         | i < k ∨ i == k ∧ j >= l = Range i j
    //         | otherwise               = Range k l
    override def add(a: LeftLong, b: LeftLong): LeftLong = {
      (a, b) match {
        case (NoLeftLong, x) => x
        case (x, NoLeftLong) => x
        case (LeftLong_(x), LeftLong_(y)) => {
          def leftlong(r1: Range, r2: Range): Range = {
            (r1, r2) match {
              case (NoRange, NoRange) => NoRange
              case (NoRange, Range_(i, j)) => Range_(i, j)
              case (Range_(i, j), NoRange) => Range_(i, j)
              case (Range_(i, j), Range_(k, l)) if (i < k) ∨ ((i == k) ∧ (j >= l)) => Range_(i, j)
              case (Range_(i, j), Range_(k, l)) => Range_(k, l)
            }
          }
          LeftLong_(leftlong(x, y))
        }
      }
    }

    // LeftLong x ⊗ LeftLong y = LeftLong (range x y)
    //   where range ...
    //         range (Range i _ ) (Range _ j) = Range i j
    override def multiply(a: LeftLong, b: LeftLong): LeftLong = {
      (a, b) match {
        case (NoLeftLong, _) => NoLeftLong
        case (_, NoLeftLong) => NoLeftLong
        case (LeftLong_(x), LeftLong_(y)) => {
          def range(r1: Range, r2: Range): Range = {
            (r1, r2) match {
              case (NoRange, NoRange)           => NoRange
              case (NoRange, Range_(i, j))      => Range_(i, j)
              case (Range_(i, j), NoRange)      => Range_(i, j)
              case (Range_(i, _), Range_(_, j)) => Range_(i, j)
            }
          }
          LeftLong_(range(x, y))
        }
      }
    }

  }

  // instance Semiringi LeftLong where
  //   index i = LeftLong (Range i i)
  trait LeftLong_Semiringi extends Semiringi[LeftLong] with LeftLong_Semiring {
    override def index(i: Int): LeftLong = {
      LeftLong_(Range_(i, i))
    }
  }
}

object Act3_Scene2_Eager_Repl extends Act3_Scene2_Eager with Act1_Scene2 {

  def repl[A](): Unit = {

    implicit object BooleanInstance extends Boolean_Semiring

    // a{n}b{n}

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let b = symw (’b’==)
    val b = symw({ (c: Char) => c == 'b' })

    // ghci> let anbn = epsw ‘alt‘ seq a (anbn ‘seq‘ b)
    def anbn(): REGw[Char, Boolean] = alt(epsw(), seq(a, seq(anbn(), b)))

    println("REMEMBER: Scala is eager so this is going to promptly overflow the stack!")

    // ghci> matchw anbn ""
    // True
    println(matchw(anbn, "".toList))

    // ghci> matchw anbn "ab"
    // True
    println(matchw(anbn, "ab".toList))

    // ghci> matchw anbn "aabb"
    // True
    println(matchw(anbn, "aabb".toList))

    // ghci> matchw anbn "aabbb"
    // False
    println(matchw(anbn, "aabbb".toList))
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}

