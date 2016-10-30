package regexp.play

import regexp.play.Semirings._

object Act3_Scene1_Repl extends Act2_Scene2 with Act1_Scene2 {

  def repl() = {

    implicit object BooleanInstance extends Boolean_Semiring

    // ghci> let a = symw (’a’==)
    val a = symw({ (c: Char) => c == 'a' })

    // ghci> let b = symw (’b’==)
    val b = symw({ (c: Char) => c == 'b' })

    // ghci> let anbn = epsw ‘altw‘ seqw a (anbn ‘seqw‘ b)
    def anbn(): REGw[Char, Boolean] = altw(epsw(), seqw(a, seqw(anbn(), b)))

    // ghci> matchw anbn ""
    matchw(anbn, "".toList)
  }

  def main(args: Array[String]): Unit = {
    println("start")
    repl()
    println("done")
  }

}
