package regexp.api

object Validator {
  def accept[C, S](r: State[C, S], u: List[C])(implicit semi: Semiring[S]): S = {

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

    def parts[A](as: List[A]): List[List[List[A]]] = {
      as match {
        case Nil     => List(Nil)
        case List(c) => List(List(List(c)))
        case c :: cs =>
          {
            for {
              p :: ps <- parts(cs)
            } yield {
              List((c :: p) :: ps, List(c) :: p :: ps)
            }
          }.flatten
      }
    }

    def sum(ss: List[S])(implicit semi: Semiring[S]): S = {
      ss.foldRight(semi.zero)(semi.add(_, _))
    }
    def prod(ss: List[S])(implicit semi: Semiring[S]): S = {
      ss.foldRight(semi.one)(semi.multiply(_, _))
    }

    r.component.get match {
      case Empty => if (u.isEmpty) semi.one else semi.zero
      case Symbol(f) => u match {
        case List(c) => {
          val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
          f1(c)
        }
        case _ => semi.zero
      }
      case Alternative(p, q) => semi.add(accept(p.get, u), accept(q.get, u))
      case Sequence(p, q) =>
        sum {
          for {
            (u1, u2) <- split(u)
          } yield {
            semi.multiply(accept(p.get, u1), accept(q.get, u2))
          }
        }
      case Repetition(r) =>
        sum {
          for {
            ps <- parts(u)
          } yield {
            prod {
              for {
                ui <- ps
              } yield {
                accept(r.get, ui)
              }
            }
          }
        }
    }
  }
}
