package regexp.api.semirings

import regexp.api.Semiring
import regexp.api.IndexedSemiring

sealed trait Leftmost
case object NoLeft extends Leftmost
case class SomeLeftmost(s: Start) extends Leftmost

sealed trait Start
case object NoStart extends Start
case class SomeStart(i: Int) extends Start

trait Leftmost_Semiring extends Semiring[Leftmost] {
  override val zero: Leftmost = NoLeft
  override val one: Leftmost = SomeLeftmost(NoStart)

  override def add(a: Leftmost, b: => Leftmost): Leftmost = {
    (a, b) match {
      case (NoLeft, x) => x
      case (x, NoLeft) => x
      case (SomeLeftmost(x), SomeLeftmost(y)) => {
        SomeLeftmost {
          (x, y) match {
            case (NoStart, NoStart)           => NoStart
            case (NoStart, s: SomeStart)      => s
            case (s: SomeStart, NoStart)      => s
            case (SomeStart(i), SomeStart(j)) => SomeStart(Math.min(i, j))
          }
        }
      }
    }
  }

  override def multiply(a: Leftmost, b: => Leftmost): Leftmost = {
    (a, b) match {
      case (NoLeft, _) => NoLeft
      case (_, NoLeft) => NoLeft
      case (SomeLeftmost(x), SomeLeftmost(y)) => {
        SomeLeftmost {
          (x, y) match {
            case (NoStart, s) => s
            case (s, _)       => s
          }
        }
      }
    }
  }
}

trait Leftmost_IndexedSemiring extends IndexedSemiring[Leftmost] with Leftmost_Semiring {
  override def index(i: Int): Leftmost = {
    SomeLeftmost(SomeStart(i))
  }
}
