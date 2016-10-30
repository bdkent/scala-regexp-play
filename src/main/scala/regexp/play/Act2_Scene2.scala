package regexp.play

import regexp.play.HaskellBridge._

trait Act2_Scene2 extends Act2_Scene1_Weighted {
  import Semirings._

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

object Act2_Scene2_Repl extends Act2_Scene2 {

  def repl[A](implicit s: Semiringi[A]): Unit = {
    // a(a|b)∗a

    // ghci> let a = symi ’a’
    val a = symi('a')

    // ghci> let ab = repw (a ‘altw‘ symi ’b’)
    val ab = repw(altw(a, symi('b')))

    // ghci> let aaba = a ‘seqw‘ ab ‘seqw‘ a
    val aaba = seqw(seqw(a, ab), a)

    // ghci> submatchw aaba "ab" :: Leftmost
    println("\t" + submatchw(aaba, "ab".toList))

    // ghci> submatchw aaba "aa" :: Leftmost
    println("\t" + submatchw(aaba, "aa".toList))

    // ghci> submatchw aaba "bababa" :: Leftmost
    println("\t" + submatchw(aaba, "bababa".toList))
  }

  def main(args: Array[String]): Unit = {
    object LeftmostInstance extends Leftmost_Semiringi
    object LeftLongInstance extends LeftLong_Semiringi

    println("Session #1 (Leftmost):")
    println("Expected:")
    println("\tNoLeft")
    println("\tLeftmost (Start 0)")
    println("\tLeftmost (Start 1)")
    println("Actual:")
    repl(LeftmostInstance)

    println("")

    println("Session #2 (LeftLong):")
    println("Expected:")
    println("\tNoLeftLong")
    println("\tLeftLong (Range 0 1)")
    println("\tLeftLong (Range 1 5)")
    println("Actual:")
    repl(LeftLongInstance)
  }
}
