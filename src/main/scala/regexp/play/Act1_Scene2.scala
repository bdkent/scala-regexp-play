package regexp.play

import regexp.play.HaskellBridge._

object Semirings {
  // class Semiring s where
  //   zero, one :: s
  //   (⊕), (⊗) :: s → s → s
  trait Semiring[S] {
    def zero(): S
    def one(): S
    def add(a: S, b: S): S
    def multiply(a: S, b: S): S
  }
  // NOTE: some implicit sugar for symbols
  implicit class SemiringOps[S: Semiring](a: S) {
    def +(b: S) = {
      val semi = implicitly[Semiring[S]]
      semi.add(a, b)
    }
    def *(b: S) = {
      val semi = implicitly[Semiring[S]]
      semi.multiply(a, b)
    }
  }

  // instance Semiring Bool where zero = False
  // one = True
  // (⊕) = (∨)
  // (⊗) = (∧)
  trait Boolean_Semiring extends Semiring[Boolean] {
    override val zero = false
    override val one = true
    override def add(a: Boolean, b: Boolean) = a ∨ b
    override def multiply(a: Boolean, b: Boolean) = a ∧ b
  }
}

trait Act1_Scene2 extends Act1_Scene1 {
  import Semirings._

  // data Regw c s = Epsw
  //               | Symw (c → s)
  //               | Altw (Regw c s) (Regw c s)
  //               | Seqw (Regw c s) (Regw c s)
  //               | Repw (Regw c s)
  trait Regw[C, S]
  case class Epsw[C, S]() extends Regw[C, S]
  case class Symw[C, S](f: (C => S)) extends Regw[C, S]
  case class Altw[C, S](p: Regw[C, S], q: Regw[C, S]) extends Regw[C, S]
  case class Seqw[C, S](p: Regw[C, S], q: Regw[C, S]) extends Regw[C, S]
  case class Repw[C, S](r: Regw[C, S]) extends Regw[C, S]

  // instance Semiring Int where
  // zero = 0
  // one = 1
  // (⊕) = (+)
  // (⊗) = (∗)
  trait Int_Semiring extends Semiring[Int] {
    override val zero = 0
    override val one = 1
    override def add(a: Int, b: Int) = a + b
    override def multiply(a: Int, b: Int) = a * b
  }

  // sym :: Semiring s ⇒ Char → Regw Char s
  // sym c = Symw (λx → if x==c then one else zero)
  def sym[S](c: Char)(implicit semi: Semiring[S]): Regw[Char, S] = {
    import semi._
    Symw { x =>
      if (x == c) one else zero
    }
  }

  // weighted :: Semiring s ⇒ Reg → Regw Char s
  // weighted Eps = Epsw
  // weighted (Sym c) = sym c
  // weighted (Alt p q) = Altw (weighted p) (weighted q)
  // weighted (Seq p q) = Seqw (weighted p) (weighted q)
  // weighted (Rep p) = Repw (weighted p)
  def weighted[S](r: Reg)(implicit semi: Semiring[S]): Regw[Char, S] = {
    r match {
      case Eps       => Epsw()
      case Sym(c)    => sym(c)
      case Alt(p, q) => Altw(weighted(p), weighted(q))
      case Seq(p, q) => Seqw(weighted(p), weighted(q))
      case Rep(p)    => Repw(weighted(p))
    }
  }

  // acceptw ::Semiring s ⇒ Regw c s → [c] → s
  // acceptw Epsw u = if null u then one else zero
  // acceptw (Symw f) u = case u of [c] → f c; _ → zero
  // acceptw (Altw p q) u = acceptw p u ⊕ acceptw q u
  // acceptw (Seqw p q) u =
  //   sum [acceptw p u1 ⊗ acceptw q u2 | (u1, u2) ← split u]
  // acceptw (Repw r) u =
  //   sum [prod [acceptw r ui | ui ← ps] | ps ← parts u]
  def acceptw[C, S](r: Regw[C, S], u: List[C])(implicit semi: Semiring[S]): S = {
    import semi._
    r match {
      case Epsw() => if (`null`(u)) one else zero
      case Symw(f) => u match {
        case List(c) => f(c)
        case _       => zero
      }
      case Altw(p, q) => acceptw(p, u) + acceptw(q, u)
      case Seqw(p, q) =>
        sum {
          for {
            (u1, u2) <- split(u)
          } yield {
            acceptw(p, u1) * acceptw(q, u2)
          }
        }
      case Repw(r) =>
        sum {
          for {
            ps <- parts(u)
          } yield {
            prod {
              for {
                ui <- ps
              } yield {
                acceptw(r, ui)
              }
            }
          }
        }
    }
  }

  // sum, prod :: Semiring s ⇒ [s] → s
  // sum = foldr (⊕) zero
  // prod = foldr (⊗) one
  def sum[S](ss: List[S])(implicit semi: Semiring[S]): S = {
    import semi._
    foldr(add, zero, ss)
  }
  def prod[S](ss: List[S])(implicit semi: Semiring[S]): S = {
    import semi._
    foldr(multiply, one, ss)
  }

}

object Act1_Scene2_Repl extends Act1_Scene2 {
  def main(args: Array[String]): Unit = {

    // ghci> let as = Alt (Sym ’a’) (Rep (Sym ’a’))
    val as = Alt(Sym('a'), Rep(Sym('a')))

    // ghci> acceptw (weighted as) "a" :: Int
    // 2
    println {
      implicit object Instance extends Int_Semiring
      acceptw(weighted(as), "a".toList)
    }

    // ghci> let bs = Alt (Sym ’b’) (Rep (Sym ’b’))
    val bs = Alt(Sym('b'), Rep(Sym('b')))

    // ghci> acceptw (weighted (Seq as bs)) "ab" :: Int
    // 4
    println {
      implicit object Instance extends Int_Semiring
      acceptw(weighted(Seq(as, bs)), "ab".toList)
    }

    // ghci> acceptw (weighted (Rep Eps)) "" :: Int
    // 1
    println {
      implicit object Instance extends Int_Semiring
      acceptw(weighted(Rep(Eps)), "".toList)
    }
  }
}

