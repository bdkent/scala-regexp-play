package regexp.api

trait Semiring[S] {
  def zero(): S
  def one(): S
  def add(a: S, b: => S): S
  def multiply(a: S, b: => S): S
}

trait IndexedSemiring[S] extends Semiring[S] {
  def index(i: Int): S
}
