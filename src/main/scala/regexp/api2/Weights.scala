package regexp.api2

import regexp.api.Semiring
import regexp.api.semirings.LeftLong
import regexp.api.semirings.Leftmost
import regexp.api.semirings.SomeLeftLong
import regexp.api.semirings.SomeLeftmost
import regexp.api.semirings.SomeRange
import regexp.api.semirings.SomeStart

trait Weight[A, B, W] {
  def symWeight(x: A => W, y: B): W
}

trait Boolean_Weight[C] extends Weight[C, C, Boolean] {
  override def symWeight(x: C => Boolean, y: C): Boolean = x(y)
}

trait Int_Weight[C] extends Weight[C, C, Int] {
  override def symWeight(x: C => Int, y: C): Int = x(y)
}

class LeftLong_Weight[C](implicit semi: Semiring[LeftLong]) extends Weight[C, (C, Int), LeftLong] {
  override def symWeight(p: C => LeftLong, t: (C, Int)): LeftLong = {
    val (c, n) = t
    semi.multiply(p(c), SomeLeftLong(SomeRange(n, n)))
  }
}

class LeftLong_Weight2[C](implicit semi: Semiring[LeftLong]) extends Weight[(C, Int), (C, Int), LeftLong] {
  override def symWeight(p: ((C, Int)) => LeftLong, t: (C, Int)): LeftLong = {
    val (c, n) = t
    semi.multiply(p(t), SomeLeftLong(SomeRange(n, n)))
  }
}

class Leftmost_Weight[C](implicit semi: Semiring[Leftmost]) extends Weight[C, (C, Int), Leftmost] {
  override def symWeight(p: C => Leftmost, t: (C, Int)): Leftmost = {
    val (c, n) = t
    semi.multiply(p(c), SomeLeftmost(SomeStart(n)))
  }
}

class Leftmost_Weight2[C](implicit semi: Semiring[Leftmost]) extends Weight[(C, Int), (C, Int), Leftmost] {
  override def symWeight(p: ((C, Int)) => Leftmost, t: (C, Int)): Leftmost = {
    val (c, n) = t
    semi.multiply(p(t), SomeLeftmost(SomeStart(n)))
  }
}
