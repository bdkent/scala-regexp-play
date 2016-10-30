package regexp.play

import regexp.play.HaskellBridge._

trait Act2_Scene1_Unweighted {

  // data REG = EPS
  //          | SYM Bool Char
  //          | ALT REG REG
  //          | SEQ REG REG
  //          | REP REG
  sealed trait REG
  case object EPS extends REG
  case class SYM(m: Boolean, c: Char) extends REG
  case class ALT(p: REG, q: REG) extends REG
  case class SEQ(p: REG, q: REG) extends REG
  case class REP(r: REG) extends REG

  // shift :: Bool → REG → Char → REG
  // shift _  EPS      _ = EPS
  // shift m (SYM _ x) c = SYM (m ∧ x == c) x
  // shift m (ALT p q) c = ALT (shift m p c) (shift m q c)
  // shift m (SEQ p q) c =
  //   SEQ (shift m p c)
  //       (shift (m ∧ empty p ∨ final p) q c)
  // shift m (REP r) c = REP (shift (m ∨ final r) r c)
  def shift(m: Boolean, r: REG, c: Char): REG = {
    (m, r, c) match {
      case (_, EPS, _)       => EPS
      case (m, SYM(_, x), c) => SYM(m ∧ (x == c), x)
      case (m, ALT(p, q), c) => ALT(shift(m, p, c), shift(m, q, c))
      case (m, SEQ(p, q), c) => SEQ(shift(m, p, c), shift((m ∧ empty(p)) ∨ `final`(p), q, c))
      case (m, REP(r), c)    => REP(shift(m ∨ `final`(r), r, c))
    }
  }

  // empty :: REG → Bool
  // empty EPS       = True
  // empty (SYM _ _) = False
  // empty (ALT p q) = empty p ∨ empty q
  // empty (SEQ p q) = empty p ∧ empty q
  // empty (REP r)   = True
  def empty(r: REG): Boolean = {
    r match {
      case EPS       => true
      case SYM(_, _) => false
      case ALT(p, q) => empty(p) ∨ empty(q)
      case SEQ(p, q) => empty(p) ∧ empty(q)
      case REP(r)    => true
    }
  }

  // final :: REG → Bool
  // final  EPS      = False
  // final (SYM b _) = b
  // final (ALT p q) = final p ∨ final q
  // final (SEQ p q) = final p ∧ empty q ∨ final q
  // final (REP r)   = final r
  def `final`(r: REG): Boolean = {
    r match {
      case EPS       => false
      case SYM(b, _) => b
      case ALT(p, q) => `final`(p) ∨ `final`(q)
      case SEQ(p, q) => (`final`(p) ∧ empty(q)) ∨ `final`(q)
      case REP(r)    => `final`(r)
    }
  }

  // match :: REG → String → Bool
  // match r [ ] = empty r
  // match r (c:cs) =
  //   final (foldl (shift False) (shift True r c) cs)
  def `match`(r: REG, s: List[Char]): Boolean = {
    s match {
      case Nil => empty(r)
      case c :: cs => {
        def f(r1: REG, c1: Char) = shift(false, r1, c1)
        `final`(foldl(f, shift(true, r, c), cs))
      }
    }
  }
}
