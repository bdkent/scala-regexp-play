package regexp.api.semirings

import regexp.api.Semiring

trait Int_Semiring extends Semiring[Int] {
  override val zero = 0
  override val one = 1
  override def add(a: Int, b: => Int) = a + b
  override def multiply(a: Int, b: => Int) = a * b
}
