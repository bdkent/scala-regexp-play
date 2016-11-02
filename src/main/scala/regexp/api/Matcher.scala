package regexp.api

object StringMatcher {
  def run[S](r: Lazy[State[Char, S]], str: String)(implicit semi: Semiring[S]): S = {
    Matcher.run[Char, S](r, str.toList)
  }
}

object Matcher {
  def run[C, S](r: Lazy[State[C, S]], xs: List[C])(implicit semi: Semiring[S]): S = {
    xs match {
      case Nil => {
        r.get.emptyWeight
      }
      case c :: cs => {
        cs.foldLeft(shift(semi.one, r, c))(shift(semi.zero, _, _)).get.finalWeight
      }
    }
  }

  private def step[C, S](m: S, r: Component[C, S], c: C)(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    r match {
      case Empty => Components.empty()
      case Symbol(f) => {
        Lazy {
          val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
          val fin = semi.multiply(m, f1(c))
          Components.symbol(f1).get.copy(active = fin != semi.zero, finalWeight = fin)
        }
      }
      case Alternative(p, q) => {
        Components.alternative(shift(m, p, c), shift(m, q, c))
      }
      case Sequence(p, q) => {
        Components.sequence(shift(m, p, c), shift(semi.add(semi.multiply(m, p.get.emptyWeight), p.get.finalWeight), q, c))
      }
      case Repetition(r) => {
        Components.repetition(shift(semi.add(m, r.get.finalWeight), r, c))
      }
    }
  }

  private def shift[C, S](m: S, r: Lazy[State[C, S]], c: C)(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    val r_ = r.get
    if (r_.active || (m != semi.zero)) {
      step(m, r_.State, c)
    } else {
      r
    }
  }
}
