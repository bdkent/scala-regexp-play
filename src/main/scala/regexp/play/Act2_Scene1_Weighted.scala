package regexp.play

import regexp.play.HaskellBridge._

trait Act2_Scene1_Weighted {
  import Semirings._

  // data REGw c s = REGw { emptyw :: s,
  //                        finalw :: s,
  //                        regw   :: REw c s }
  case class REGw[+C, +S](emptyw: S, finalw: S, regw: REw[C, S])
  def emptyw[C, S](r: REGw[C, S]): S = r.emptyw
  def finalw[C, S](r: REGw[C, S]): S = r.finalw
  def regw[C, S](r: REGw[C, S]): REw[C, S] = r.regw

  // data REw c s = EPSw
  //              | SYMw (c → s)
  //              | ALTw (REGw c s)(REGw c s)
  //              | SEQw (REGw c s)(REGw c s)
  //              | REPw (REGw c s)
  sealed trait REw[+C, +S]
  case class EPSw[+C, +S]() extends REw[C, S]
  case class SYMw[C, +S](x0: (C => S)) extends REw[C, S]
  case class ALTw[+C, +S](x0: REGw[C, S], x1: REGw[C, S]) extends REw[C, S]
  case class SEQw[+C, +S](x0: REGw[C, S], x1: REGw[C, S]) extends REw[C, S]
  case class REPw[+C, +S](x0: REGw[C, S]) extends REw[C, S]

  // epsw :: Semiring s ⇒ REGw c s
  // epsw = REGw { emptyw = one,
  //               finalw = zero,
  //               regw   = EPSw }
  def epsw[C, S]()(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = one,
      finalw = zero,
      regw = EPSw())
  }

  // symw :: Semiring s ⇒ (c → s) → REGw c s
  // symw f = REGw { emptyw = zero,
  //                 finalw = zero,
  //                 regw   = SYMw f }
  def symw[C, S](f: (C => S))(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = zero,
      finalw = zero,
      regw = SYMw(f))
  }

  // altw :: Semiring s ⇒ REGw c s → REGw c s → REGw c s
  // altw p q = REGw { emptyw = emptyw p ⊕ emptyw q,
  //                   finalw = finalw p ⊕ finalw q,
  //                   regw   = ALTw p q }
  def altw[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    REGw(
      emptyw = emptyw(p) + emptyw(q),
      finalw = finalw(p) + finalw(q),
      regw = ALTw(p, q))
  }

  // seqw ::Semiring s ⇒ REGw c s → REGw c s → REGw c s
  // seqw p q =
  //   REGw { emptyw = emptyw p ⊗ emptyw q,
  //          finalw = finalw p ⊗ emptyw q ⊕ finalw q,
  //          regw = SEQw p q }
  def seqw[C, S](p: REGw[C, S], q: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    REGw(
      emptyw = emptyw(p) * emptyw(q),
      finalw = (finalw(p) * emptyw(q)) + finalw(q),
      regw = SEQw(p, q))
  }

  // repw :: Semirings ⇒ REGw c s → REGw c s
  // repw r = REGw { emptyw = one,
  //                 finalw = finalw r,
  //                 regw   = REPw r }
  def repw[C, S](r: REGw[C, S])(implicit semi: Semiring[S]): REGw[C, S] = {
    import semi._
    REGw(
      emptyw = one,
      finalw = finalw(r),
      regw = REPw(r))
  }

  // matchw :: Semiring s ⇒ REGw c s → [c] → s
  // matchw r [ ]      = emptyw r
  // matchw r (c : cs) =
  //  finalw (foldl (shiftw zero · regw) (shiftw one (regw r) c) cs)
  def matchw[C, S](r: REGw[C, S], xs: List[C])(implicit semi: Semiring[S]): S = {
    import semi._
    xs match {
      case Nil => emptyw(r)
      case c :: cs =>
        def f(r1: REGw[C, S], c1: C): REGw[C, S] = shiftw(zero, regw(r1), c1)
        finalw(foldl(f, shiftw(one, regw(r), c), cs))
    }
  }

  // shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
  // shiftw _  EPSw      _ = epsw
  // shiftw m (SYMw f)   c = (symw f) {finalw = m ⊗ f c}
  // shiftw m (ALTw p q) c =
  //   altw (shiftw m (regw p) c) (shiftw m (regw q) c)
  // shiftw m (SEQw p q) c =
  //   seqw (shiftw m (regw p) c)
  //        (shiftw (m ⊗ emptyw p ⊕ finalw p) (regw q) c)
  // shiftw m (REPw r)   c =
  //   repw (shiftw (m ⊕ finalw r) (regw r) c)
  def shiftw[C, S](m: S, r: REw[C, S], c: C)(implicit semi: Semiring[S]): REGw[C, S] = {
    r match {
      case EPSw() => epsw()
      case SYMw(f) => {
        val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
        symw(f).copy(finalw = m * f1(c))
      }
      case ALTw(p, q) => altw(shiftw(m, regw(p), c), shiftw(m, regw(q), c))
      case SEQw(p, q) => seqw(shiftw(m, regw(p), c), shiftw(m * emptyw(p) + finalw(p), regw(q), c))
      case REPw(r)    => repw(shiftw(m + finalw(r), regw(r), c))
    }
  }
}
