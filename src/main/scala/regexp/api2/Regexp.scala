package regexp.api2

import regexp.api.Semiring
import regexp.api.semirings.Boolean_Semiring

//trait RegExp[C] {
//  type W
//  val reg: RegW[W, C]
//  def semiring: Semiring[W]
//}
//
//object RegExp {
//  def apply[W1, C](r: RegW[W1, C]): RegExp[C] = {
//    new RegExp[C] {
//      type W = W1
//      val reg = r
//    }
//  }
//}

class RegW[+W, +C](a: => Boolean, e: => W, f: => W, r: => Reg[W, C]) {
  lazy val active = a
  lazy val empty = e
  lazy val final_ = f
  lazy val reg = r
}

sealed trait Reg[+W, +C]
case object Eps extends Reg[Nothing, Nothing]
case class Sym[W, C](s: List[Char], p: C => W) extends Reg[W, C]
case class Alt[W, C](p: RegW[W, C], q: RegW[W, C]) extends Reg[W, C]
case class Seq[W, C](p: RegW[W, C], q: RegW[W, C]) extends Reg[W, C]
case class Rep[W, C](r: RegW[W, C]) extends Reg[W, C]

object Regex {

  def `final`[W, C](r: RegW[W, C])(implicit semi: Semiring[W]): W = {
    if (r.active) { r.final_ } else { semi.zero }
  }

  def weighted[W, A, B](reg: RegW[W, A])(implicit weight: Weight[A, B, W]): RegW[W, B] = {
    val a = reg.active
    val e = reg.empty
    val f = reg.final_
    val r = reg.reg

    r match {
      case Eps       => new RegW(a, e, f, Eps)
      case Sym(s, p) => new RegW(a, e, f, Sym(s, { (x: B) => weight.symWeight(p.asInstanceOf[A => W], x) }))
      case Alt(p, q) => new RegW(a, e, f, Alt(weighted(p), weighted(q)))
      case Seq(p, q) => new RegW(a, e, f, Seq(weighted(p), weighted(q)))
      case Rep(p)    => new RegW(a, e, f, Rep(weighted(p)))
    }
  }

  def eps[W, C]()(implicit semi: Semiring[W]): RegW[W, C] = {
    epsW()
  }

  def epsW[W, C]()(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(false, semi.one, semi.zero, Eps)
  }

  def char[W](c: Char): RegW[Boolean, Char] = {
    psym(quote(c), { (x: Char) => c == x })
  }

  def sym[W, C](c: C): RegW[Boolean, C] = {
    psym(c.toString.toList, { (x: C) => c == x })
  }

  def quote(c: Char): List[Char] = {
    c match {
      case '\\' | '|' | '*' | '+' | '?' | '.' | '[' | ']' | '{' | '}' | '^' => List('\\', c)
      case _ => List(c)
    }
  }

  def psym[C](s: List[Char], p: C => Boolean): RegW[Boolean, C] = {
    implicit object Boolean_Instance extends Boolean_Semiring
    symW(s, p)
  }

  def symW[W, C](s: List[Char], p: C => W)(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(false, semi.zero, semi.zero, Sym(s, p))
  }

  def constantTrue[A](a: A): Boolean = true

  def anySym[W, C](): RegW[Boolean, C] = {
    psym(List('.'), constantTrue)
  }

  def alt[W, C](p: => RegW[W, C], q: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(false, semi.add(p.empty, q.empty), semi.zero, Alt(p, q))
  }

  def altW[W, C](p: => RegW[W, C], q: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(p.active || q.active, semi.add(p.empty, q.empty), semi.add(`final`(p), `final`(q)), Alt(p, q))
  }

  def seq_[W, C](p: => RegW[W, C], q: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(false, semi.multiply(p.empty, q.empty), semi.zero, Seq(p, q))
  }

  def seqW[W, C](p: => RegW[W, C], q: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(p.active || q.active, semi.multiply(p.empty, q.empty),
      semi.add(semi.multiply(`final`(p), q.empty), `final`(q)),
      Seq(p, q))
  }

  def rep[W, C](r: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(false, semi.one, semi.zero, Rep(r))
  }

  def repW[W, C](r: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    new RegW(r.active, semi.one, `final`(r), Rep(r))
  }

  def rep1[W, C](r: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    seq_(r, rep(r))
  }

  def opt[W, C](r: => RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    alt(eps(), r)
  }

  def brep[W, C](n: Int, m: Int, r: RegW[W, C])(implicit semi: Semiring[W]): RegW[W, C] = {
    if (n < 0 || m > 0 || n > m) {
      ???
    } else {
      if (n == 0 || m == 0) {
        eps()
      } else {
        if (n == m) {
          List.fill(n)(r).reduce({ (p, q) => seq_(p, q) })
        } else {
          val rest = List.fill(m - n - 1)(r).foldRight(opt(r))({ (p, q) => opt(seq_(p, q)) })
          List.fill(n)(r).foldLeft(rest)({ (p, q) => seq_(p, q) })
        }
      }
    }
  }

}
