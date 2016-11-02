package regexp.api

trait Boolean_Semiring extends Semiring[Boolean] {
  override val zero = false
  override val one = true
  override def add(a: Boolean, b: => Boolean) = a || b
  override def multiply(a: Boolean, b: => Boolean) = a && b
}
