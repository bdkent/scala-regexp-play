package regexp.play

trait HaskellBridge {

  def `null`[A](xs: List[A]): Boolean = xs.isEmpty

  def or(bs: List[Boolean]): Boolean = bs.foldLeft(false)(_ || _)

  def and(bs: List[Boolean]): Boolean = bs.foldLeft(true)(_ && _)

  def concat[A](as: List[List[A]]): List[A] = as.flatten

  def foldr[A, B](op: ((A, B) => B), z: B, xs: List[A]): B = xs.foldRight(z)({ (a, b) => op(a, b) })

  def foldl[A, B](op: ((A, B) => A), z: A, xs: List[B]): A = xs.foldLeft(z)({ (a, b) => op(a, b) })

  def min(a: Int, b: Int): Int = Math.min(a, b)

  def zipWithIndex[A](xs: List[A]): List[(Int, A)] = xs.zipWithIndex.map({ case (e, i) => (i, e) })

  def foldr1[A](op: ((A, A) => A), xs: List[A]): A = xs.reduce({ (a, b) => op(a, b) })

  def replicate[A](n: Int, x: A): List[A] = List.fill(n)(x)

  implicit class BooleanOps(a: Boolean) {
    def ∨(b: => Boolean): Boolean = a || b
    def ∧(b: => Boolean): Boolean = a && b
  }
}

object HaskellBridge extends HaskellBridge
