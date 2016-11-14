package regexp.api2

import regexp.api.Semiring

object Matcher {

  import Regex._

  def matchW[W, C](r: RegW[W, C], xs: List[C])(implicit semi: Semiring[W]): W = {
    xs match {
      case Nil     => r.empty
      case c :: cs => `final`(cs.foldLeft(shiftW(semi.one, r, c))({ (r1, c1) => shiftW(semi.zero, r1, c1) }))
    }
  }

  def shiftW[W, C](w: W, r: RegW[W, C], c: C)(implicit semi: Semiring[W]): RegW[W, C] = {
    if (r.active || semi.zero != w) { shift(w, r.reg, c) } else { r }
  }

  def shift[W, C](w: W, r: Reg[W, C], c: C)(implicit semi: Semiring[W]): RegW[W, C] = {
    r match {
      case Eps => epsW()
      case Sym(s, f) => {
        lazy val w_ = semi.multiply(w, f.asInstanceOf[C => W](c))
        lazy val s2 = symW(s, f)
        new RegW(w_ != semi.zero, s2.empty, w_, s2.reg)
      }
      case Alt(p, q) => altW(shiftW(w, p, c), shiftW(w, q, c))
      case Seq(p, q) => seqW(shiftW(w, p, c), shiftW(semi.add(semi.multiply(w, p.empty), `final`(p)), q, c))
      case Rep(r)    => repW(shiftW(semi.add(w, `final`(r)), r, c))
    }
  }

  def acceptFull[W, C](r: RegW[W, C], cs: List[C])(implicit semi: Semiring[W], weight: Weight[C, C, W]): W = {
    fullMatch(r, cs)
  }

  def acceptPartial[W, C](r: RegW[W, C], cs: List[C])(implicit semi: Semiring[W], weight: Weight[C, C, W]): W = {
    partialMatch(r, cs)
  }

  def matchingCount[C](r: RegW[Int, C], cs: List[C])(implicit semi: Semiring[Int], weight: Weight[C, C, Int]): Int = {
    fullMatch(r, cs)
  }

  def fullMatch[C, W](r: RegW[W, C], bs: List[C])(implicit semi: Semiring[W], weight: Weight[C, C, W]): W = {
    matchW(weighted(r), bs)
  }

  def partialMatch[A, W](r: RegW[W, A], bs: List[A])(implicit semi: Semiring[W], weight: Weight[A, A, W]): W = {
    def f(x: A) = semi.one
    lazy val arb = rep(symW(Nil, f _))
    matchW(seqW(arb, weighted(seqW(r, arb))), bs)
  }
}
