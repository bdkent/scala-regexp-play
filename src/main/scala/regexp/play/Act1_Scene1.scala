package regexp.play

import regexp.play.HaskellBridge._

trait Act1_Scene1 {

  // data Reg = Eps            -- ε
  //          | Sym Char       -- α
  //          | Alt Reg Reg	   -- α|β
  //          | Seq Reg Reg    -- αβ
  //          | Rep Reg        -- α∗
  sealed trait Reg
  case object Eps extends Reg
  case class Sym(c: Char) extends Reg
  case class Alt(p: Reg, q: Reg) extends Reg
  case class Seq(p: Reg, q: Reg) extends Reg
  case class Rep(r: Reg) extends Reg

  // accept :: Reg → String → Bool
  // accept Eps u = null u
  // accept (Sym c) u = u == [ c ]
  // accept (Alt p q) u = accept p u \/ accept q u
  // accept (Seq p q) u =
  //   or [accept p u1 /\ accept q u2 | (u1,u2) ← split u]
  // accept (Rep r) u =
  //   or[and[accept r ui | ui ← ps] | ps ← parts u]
  def accept(r: Reg, u: List[Char]): Boolean = {
    r match {
      case Eps       => `null`(u)
      case Sym(c)    => u == List(c)
      case Alt(p, q) => accept(p, u) || accept(q, u)
      case Seq(p, q) =>
        or {
          for {
            (u1, u2) <- split(u)
          } yield {
            accept(p, u1) && accept(q, u2)
          }
        }
      case Rep(r) =>
        or {
          for {
            ps <- parts(u)
          } yield {
            and {
              for {
                ui <- ps
              } yield {
                accept(r, ui)
              }
            }
          }
        }
    }
  }

  // split :: [a] → [([a], [a])]
  // split [] = [([],[])]
  // split (c : cs) = ([],c : cs) : [(c : s1,s2) | (s1,s2) ← split cs]
  def split[A](as: List[A]): List[(List[A], List[A])] = {
    as match {
      case c :: cs => (Nil, c :: cs) :: {
        for {
          (s1, s2) <- split(cs)
        } yield {
          (c :: s1, s2)
        }
      }
      case Nil => List((Nil, Nil))
    }
  }

  // parts :: [a] → [[[a]]]
  // parts [] = [[]]
  // parts [c] = [[[c]]]
  // parts (c : cs) =
  //   concat [[(c:p):ps,[c]:p:ps] | p:ps ← parts cs]
  def parts[A](as: List[A]): List[List[List[A]]] = {
    as match {
      case Nil     => List(Nil)
      case List(c) => List(List(List(c)))
      case c :: cs =>
        concat {
          for {
            p :: ps <- parts(cs)
          } yield {
            List((c :: p) :: ps, List(c) :: p :: ps)
          }
        }
    }
  }
}

object Act1_Scene1_Repl extends Act1_Scene1 {
  def main(args: Array[String]): Unit = {

    // ((a|b)*c(a|b)*c)*(a|b)

    // ghci> let nocs = Rep (Alt (Sym ’a’) (Sym ’b’))
    val nocs = Rep(Alt(Sym('a'), Sym('b')))

    // ghci> let onec = Seq nocs (Sym ’c’)
    val onec = Seq(nocs, Sym('c'))

    // ghci> let evencs = Seq (Rep (Seq onec onec)) nocs
    val evencs = Seq(Rep(Seq(onec, onec)), nocs)

    // ghci> parts "acc"
    // [["acc"],["a","cc"],["ac","c"],["a","c","c"]]
    println(parts("acc".toList))

    // ghci> accept evencs "acc"
    // True
    println(accept(evencs, "acc".toList))
  }
}
