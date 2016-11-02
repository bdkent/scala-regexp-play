package regexp.api.semirings

import regexp.api.Semiring
import regexp.api.IndexedSemiring

sealed trait LeftLong
case object NoLeftLong extends LeftLong
case class SomeLeftLong(r: Range) extends LeftLong

sealed trait Range
case object NoRange extends Range
case class SomeRange(x: Int, y: Int) extends Range

trait LeftLong_Semiring extends Semiring[LeftLong] {
  override val zero: LeftLong = NoLeftLong
  override val one: LeftLong = SomeLeftLong(NoRange)

  override def add(a: LeftLong, b: => LeftLong): LeftLong = {
    (a, b) match {
      case (NoLeftLong, x) => x
      case (x, NoLeftLong) => x
      case (SomeLeftLong(x), SomeLeftLong(y)) => {
        SomeLeftLong {
          (x, y) match {
            case (NoRange, NoRange) => NoRange
            case (NoRange, r: SomeRange) => r
            case (r: SomeRange, NoRange) => r
            case (r @ SomeRange(i, j), SomeRange(k, l)) if (i < k) || ((i == k) && (j >= l)) => r
            case (_: SomeRange, r: SomeRange) => r
          }
        }
      }
    }
  }

  override def multiply(a: LeftLong, b: => LeftLong): LeftLong = {
    (a, b) match {
      case (NoLeftLong, _) => NoLeftLong
      case (_, NoLeftLong) => NoLeftLong
      case (SomeLeftLong(x), SomeLeftLong(y)) => {
        SomeLeftLong {
          (x, y) match {
            case (NoRange, NoRange)                 => NoRange
            case (NoRange, r: SomeRange)            => r
            case (r: SomeRange, NoRange)            => r
            case (SomeRange(i, _), SomeRange(_, j)) => SomeRange(i, j)
          }
        }
      }
    }
  }

}

trait LeftLong_IndexedSemiring extends IndexedSemiring[LeftLong] with LeftLong_Semiring {
  override def index(i: Int): LeftLong = {
    SomeLeftLong(SomeRange(i, i))
  }
}
