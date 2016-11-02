package regexp.play

import regexp.play.HaskellBridge._

trait Act3_Scene2_Lazy extends LazySemirings {

  type Lazy[+A] = () => A

  object Lazy {
    def apply[A](a: => A): Lazy[A] = () => a
  }

  // added a boolean field active to the data type REGw
  case class REGw[+C, +S](active: Boolean = false, emptyw: S, finalw: S, regw: REw[C, S])

  sealed trait REw[+C, +S]
  case object EPSw extends REw[Nothing, Nothing]
  case class SYMw[C, +S](x0: (C => S)) extends REw[C, S]
  case class ALTw[+C, +S](x0: Lazy[REGw[C, S]], x1: Lazy[REGw[C, S]]) extends REw[C, S]
  case class SEQw[+C, +S](x0: Lazy[REGw[C, S]], x1: Lazy[REGw[C, S]]) extends REw[C, S]
  case class REPw[+C, +S](x0: Lazy[REGw[C, S]]) extends REw[C, S]

  def epsw[C, S]()(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        emptyw = one,
        finalw = zero,
        regw = EPSw)
    }
  }

  def symw[C, S](f: (C => S))(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        emptyw = zero,
        finalw = zero,
        regw = SYMw(f))
    }
  }

  def altw[C, S](p: Lazy[REGw[C, S]], q: => Lazy[REGw[C, S]])(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        active = { p().active ∨ q().active },
        emptyw = { add(p().emptyw, q().emptyw) },
        finalw = { add(finala(p())(semi), finala(q())) },
        regw = ALTw(p, q))
    }
  }

  def alt[C, S](p: => Lazy[REGw[C, S]], q: => Lazy[REGw[C, S]])(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        emptyw = { add(p().emptyw, q().emptyw) },
        finalw = zero,
        regw = ALTw(p, q))
    }
  }

  // seqw :: Semiring s ⇒ REGw c s → REGw c s → REGw c s
  // seqw p q =
  //   REGw { active = active p ∨ active q,
  //          emptyw = emptyw p ⊗ emptyw q,
  //          finalw = finala p ⊗ emptyw q ⊕ finala q,
  //          regw = SEQw p q }
  def seqw[C, S](p: => Lazy[REGw[C, S]], q: => Lazy[REGw[C, S]])(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        active = { p().active ∨ q().active },
        emptyw = { p().emptyw * q().emptyw },
        finalw = { add(multiply(finala(p()), q().emptyw), finala(q())) },
        regw = SEQw(p, q))
    }
  }

  def repw[C, S](r: => Lazy[REGw[C, S]])(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        active = { r().active },
        emptyw = { one },
        finalw = { finala(r())(semi) },
        regw = REPw(r))
    }
  }

  // finala ::Semiring s ⇒ REGw c s → s
  // finala r = if active r then finalw r else zero
  def finala[C, S](r: REGw[C, S])(implicit semi: Semiring[S]): S = {
    import semi._
    if (r.active) r.finalw else zero
  }

  // seq p q = REGw { active = False,
  //                  emptyw = emptyw p ⊗ emptyw q,
  //                  finalw = zero,
  //                  regw = SEQw p q }
  def seq[C, S](p: => Lazy[REGw[C, S]], q: => Lazy[REGw[C, S]])(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._
    Lazy {
      REGw(
        emptyw = { p().emptyw * q().emptyw },
        finalw = { zero },
        regw = SEQw(p, q))
    }
  }

  def matchw[C, S](r: Lazy[REGw[C, S]], xs: List[C])(implicit semi: Semiring[S]): S = {
    import semi._
    xs match {
      case Nil => {
        r().emptyw
      }
      case c :: cs =>
        {
          def f(r1: Lazy[REGw[C, S]], c1: C): Lazy[REGw[C, S]] = shiftw(zero, r1, c1)
          foldl(f, shiftw(one, r, c), cs)().finalw
        }
    }
  }

  // shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
  // shiftw ...
  // shiftw m (SYMw f)  c =
  //   let fin = m ⊗ f c
  //   in (symw f) {active = fin = ̸= zero, finalw = fin}
  def stepw[C, S](m: S, r: REw[C, S], c: C)(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._

    r match {
      case EPSw => epsw()
      case SYMw(f) => {
        Lazy {
          val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
          val fin = m * f1(c)
          symw(f1)(semi)().copy(
            active = fin != zero,
            finalw = fin)
        }
      }
      case ALTw(p, q) => altw(shiftw(m, p, c), shiftw(m, q, c))
      case SEQw(p, q) => seqw(shiftw(m, p, c), shiftw(add(multiply(m, p().emptyw), p().finalw), q, c))
      case REPw(r)    => repw(shiftw(add(m, r().finalw), r, c))
    }
  }

  // shiftw :: (Eq s, Semiring s) ⇒ s → REGw c s → c → REGw c s
  // shiftw m r c | active r ∨ m =/= zero = stepw m (regw r) c
  //              | otherwise            = r
  def shiftw[C, S](m: S, r: Lazy[REGw[C, S]], c: C)(implicit semi: Semiring[S]): Lazy[REGw[C, S]] = {
    import semi._

    val r_ = r()
    if (r_.active ∨ (m != zero)) { stepw(m, r_.regw, c) } else { r }
  }
}

object Act3_Scene2_Lazy_Repl extends Act3_Scene2_Lazy {

  def repl1[A](): Unit = {

    implicit object BooleanInstance extends Boolean_Semiring

    // a{n}b{n}

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let b = symw (’b’==)
    val b = symw({ (c: Char) => c == 'b' })

    // ghci> let anbn = epsw ‘alt‘ seq a (anbn ‘seq‘ b)
    lazy val anbn: Lazy[REGw[Char, Boolean]] = alt(epsw(), seq(a, seq(anbn, b)))

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

  def repl2[A](): Unit = {

    implicit object BooleanInstance extends Boolean_Semiring

    // ghci> let bs n = replicate n (symw (’b’==))
    def bs(n: Int) = replicate(n, symw({ (c: Char) => c == 'b' }))

    // ghci> let cs n = replicate n (symw (’c’==))
    def cs(n: Int) = replicate(n, symw({ (c: Char) => c == 'c' }))

    // ghci> let bcs n=foldr1 seq (bs n ++ cs n)
    def bcs(n: Int) = foldr1((p: Lazy[REGw[Char, Boolean]], q: Lazy[REGw[Char, Boolean]]) => seq(p, q), bs(n) ++ cs(n))

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let abc n = a ‘seq‘ alt (bcs n) (abc (n + 1))
    def abc(n: Int): Lazy[REGw[Char, Boolean]] = seq(a, alt(bcs(n), abc(n + 1)))

    // ghci> let anbncn = epsw ‘alt‘ abc 1
    val anbncn = alt(epsw(), abc(1))

    // ghci> matchw anbncn ""
    // True
    println(matchw(anbncn, "".toList))

    // ghci> matchw anbncn "abc"
    // True
    println(matchw(anbncn, "abc".toList))

    // ghci> matchw anbncn "aabbcc"
    // True
    println(matchw(anbncn, "aabbcc".toList))

    // ghci> matchw anbncn "aabbbcc"
    // False
    println(matchw(anbncn, "aabbbcc".toList))
  }

  def main(args: Array[String]): Unit = {
    repl1()
    repl2()
  }
}
