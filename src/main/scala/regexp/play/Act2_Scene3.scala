package regexp.play

import regexp.play.HaskellBridge._
import regexp.play.Semirings._

object Act2_Scene3_Repl extends Act2_Scene1_Weighted {

  def repl() = {

    implicit object BooleanInstance extends Boolean_Semiring

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let seqn n = foldr1 seqw . replicate n
    def seqn(n: Int) = (r: REGw[Char, Boolean]) => {
      def f(a: REGw[Char, Boolean], b: REGw[Char, Boolean]) = seqw(a, b)
      foldr1(f, replicate(n, r))
    }

    // ghci> let re n = seqn n (altw a epsw) ‘seqw‘ seqn n a
    def re[C](n: Int) = seqw(seqn(n)(altw(a, epsw())), seqn(n)(a))

    val n = 50

    // ghci> matchw (re 500) (replicate 500 ’a’)
    println(matchw(re(n), replicate(n, 'a')))

    // True
    // (5.99 secs, 491976576 bytes)
  }

  def main(args: Array[String]): Unit = {
    println("start")
    repl()
    println("done")
  }

}
