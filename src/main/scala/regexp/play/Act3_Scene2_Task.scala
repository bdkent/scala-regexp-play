package regexp.play

import regexp.play.HaskellBridge._
import regexp.play.Semirings._
import scalaz.concurrent.Task

trait Act3_Scene2_Task {

  // added a boolean field active to the data type REGw
  case class REGw[+C, +S](active: Boolean = false, emptyw: S, finalw: S, regw: REw[C, S])

  sealed trait REw[+C, +S]
  case class EPSw[+C, +S]() extends REw[C, S]
  case class SYMw[C, +S](x0: (C => S)) extends REw[C, S]
  case class ALTw[+C, +S](x0: Task[REGw[C, S]], x1: Task[REGw[C, S]]) extends REw[C, S]
  case class SEQw[+C, +S](x0: Task[REGw[C, S]], x1: Task[REGw[C, S]]) extends REw[C, S]
  case class REPw[+C, +S](x0: Task[REGw[C, S]]) extends REw[C, S]

  def epsw[C, S]()(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"epsw()")
    import semi._
    Task.delay {
      REGw(
        emptyw = one,
        finalw = zero,
        regw = EPSw())
    }
  }

  def symw[C, S](f: (C => S))(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"symw(_)")
    import semi._
    Task.delay {
      REGw(
        emptyw = zero,
        finalw = zero,
        regw = SYMw(f))
    }
  }

  def altw[C, S](p: Task[REGw[C, S]], q: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"altw(_, _)")
    for {
      pv <- p
      qv <- q
    } yield {
      REGw(
        emptyw = pv.emptyw + qv.emptyw,
        finalw = pv.finalw + qv.finalw,
        regw = ALTw(p, q))
    }
  }

  def alt[C, S](p: Task[REGw[C, S]], q: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"alt(_, _)")
    import semi._
    for {
      pv <- p
      qv <- q
    } yield {
      REGw(
        emptyw = pv.emptyw + qv.emptyw,
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
  def seqw[C, S](p: Task[REGw[C, S]], q: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"seqw(_, _)")
    for {
      pv <- p
      qv <- q
      pfa <- finala(p)
      qfa <- finala(q)
    } yield {
      REGw(
        active = pv.active ∨ qv.active,
        emptyw = pv.emptyw * qv.emptyw,
        finalw = (pfa * qv.emptyw) + qfa,
        regw = SEQw(p, q))
    }
  }

  def repw[C, S](r: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"repw(_)")
    import semi._
    for {
      rv <- r
    } yield {
      REGw(
        emptyw = one,
        finalw = rv.finalw,
        regw = REPw(r))
    }
  }

  // finala ::Semiring s ⇒ REGw c s → s
  // finala r = if active r then finalw r else zero
  def finala[C, S](r: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[S] = {
    println(s"finala(_)")
    import semi._
    for {
      rv <- r
    } yield {
      if (rv.active) rv.finalw else zero
    }
  }

  // seq p q = REGw { active = False,
  //                  emptyw = emptyw p ⊗ emptyw q,
  //                  finalw = zero,
  //                  regw = SEQw p q }
  def seq[C, S](p: Task[REGw[C, S]], q: Task[REGw[C, S]])(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"seq(_, _)")
    import semi._
    for {
      pv <- p
      qv <- q
    } yield {
      REGw(
        emptyw = pv.emptyw * qv.emptyw,
        finalw = zero,
        regw = SEQw(p, q))
    }
  }

  def matchw[C, S](r: Task[REGw[C, S]], xs: List[C])(implicit semi: Semiring[S]): Task[S] = {
    println(s"matchw(_, ${xs})")
    import semi._
    xs match {
      case Nil => {
        println(1)
        r.map({ x =>
          println(2)
          x.emptyw
        })
      }
      case c :: cs =>
        def f(r1: Task[REGw[C, S]], c1: C): Task[REGw[C, S]] = shiftw(zero, r1, c1)
        foldl(f, shiftw(one, r, c), cs).map(_.finalw)
    }
  }

  // shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
  // shiftw ...
  // shiftw m (SYMw f)  c =
  //   let fin = m ⊗ f c
  //   in (symw f) {active = fin = ̸= zero, finalw = fin}
  def stepw[C, S](m: S, r: REw[C, S], c: C)(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"stepw(${m},${r},${c})")
    import semi._

    r match {
      case EPSw() => epsw()
      case SYMw(f) => {
        val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
        val fin = m * f1(c)
        symw(f1).map(_.copy(
          active = fin != zero,
          finalw = fin))
      }
      case ALTw(p, q) => altw(shiftw(m, p, c), shiftw(m, q, c))
      case SEQw(p, q) => {
        for {
          pv <- p
          result <- seqw(shiftw(m, p, c), shiftw(m * pv.emptyw + pv.finalw, q, c))
        } yield {
          result
        }
      }
      case REPw(r) => {
        for {
          rv <- r
          result <- repw(shiftw(m + rv.finalw, r, c))
        } yield {
          result
        }
      }
    }
  }

  // shiftw :: (Eq s, Semiring s) ⇒ s → REGw c s → c → REGw c s
  // shiftw m r c | active r ∨ m =/= zero = stepw m (regw r) c
  //              | otherwise            = r
  def shiftw[C, S](m: S, r: Task[REGw[C, S]], c: C)(implicit semi: Semiring[S]): Task[REGw[C, S]] = {
    println(s"shiftw(${m}, _, ${c})")
    import semi._
    for {
      rv <- r
      result <- if (rv.active ∨ (m != zero)) stepw(m, rv.regw, c) else r
    } yield {
      result
    }
  }

  //  // submatchw :: Semiring s ⇒ REGw (Int, c) s → [c] → s
  //  // submatchw r s =
  //  //   matchw (seqw arb (seqw r arb)) (zip [0..] s)
  //  //   where arb = repw (symw (λ_ → one))
  //  def submatchw[C, S](r: Task[REGw[(Int, C), S]], s: List[C])(implicit semi: Semiring[S]): S = {
  //    def f(x: C) = semi.one
  //    val arb = repw(symw(f))
  //    matchw(seqw(arb, seqw(r, arb)), zipWithIndex(s))
  //  }

  //  // class Semiring s ⇒ Semiringi s where
  //  //   index :: Int → s
  //  trait Semiringi[S] extends Semiring[S] {
  //    def index(i: Int): S
  //  }

  //  // symi :: Semiringi s ⇒ Char → REGw (Int, Char) s
  //  // symi c = symw weight
  //  //   where weight (pos, x) | x == c    = index pos
  //  //                         | otherwise = zero
  //  def symi[S](c: Char)(implicit semi: Semiringi[S]): Task[REGw[(Int, Char), S]] = {
  //    import semi._
  //    def weight(t: (Int, Char)): S = {
  //      t match {
  //        case (pos, x) if x == c => index(pos)
  //        case (pos, x)           => zero
  //      }
  //    }
  //    symw(weight)
  //  }
}

object Act3_Scene2_Task_Repl extends Act3_Scene2_Task {

  def repl[A](): Unit = {

    implicit object BooleanInstance extends Boolean_Semiring

    // a{n}b{n}

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let b = symw (’b’==)
    val b = symw({ (c: Char) => c == 'b' })

    // ghci> let anbn = epsw ‘alt‘ seq a (anbn ‘seq‘ b)
    def anbn(): Task[REGw[Char, Boolean]] = {
      println(s"anbn()")
      Task.suspend(alt(epsw(), seq(a, seq(anbn(), b))))
    }

    println("""ghci> matchw anbn "" """)
    println("expect: True")
    println(matchw(anbn, "".toList).unsafePerformSync)

    println("""ghci> matchw anbn "ab" """)
    println("expect: True")
    println(matchw(anbn, "ab".toList).unsafePerformSync)

    println("""ghci> matchw anbn "aabb" """)
    println("expect: True")
    println(matchw(anbn, "aabb".toList).unsafePerformSync)

    println("""ghci> matchw anbn "aabbb" """)
    println("expect: False")
    println(matchw(anbn, "aabbb".toList).unsafePerformSync)

  }

  def main(args: Array[String]): Unit = {
    println("START")
    repl()
    println("DONE")
  }
}

