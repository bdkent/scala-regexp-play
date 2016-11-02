package regexp.api

object StringMatcher {
  def run[S](r: Lazy[State[Char, S]], str: String)(implicit semi: Semiring[S]): S = {
    Matcher.run[Char, S](r, str.toList)
  }

  def submatch[S](r: Lazy[State[(Char, Int), S]], str: String)(implicit semi: Semiring[S]): S = {
    Matcher.submatch[Char, S](r, str.toList)
  }
}

object Matcher {
  def run[C, S](state: Lazy[State[C, S]], xs: List[C])(implicit semi: Semiring[S]): S = {
    xs match {
      case Nil => {
        state.get.emptyWeight
      }
      case c :: cs => {
        cs.foldLeft(shift(semi.one, state, c))(shift(semi.zero, _, _)).get.finalWeight
      }
    }
  }

  def submatch[C, S](r: Lazy[State[(C, Int), S]], s: List[C])(implicit semi: Semiring[S]): S = {
    def f(x: (Int, C)) = semi.one
    lazy val arb = Components.repetition(Components.symbol(f))
    run(Components.sequence(arb, Components.sequence(r, arb)), s.zipWithIndex)
  }

  private def step[C, S](mark: S, component: Component[C, S], c: C)(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    component match {
      case Empty => Components.empty()
      case Symbol(f) => {
        Lazy {
          val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
          val fin = semi.multiply(mark, f1(c))
          Components.symbol(f1).get.copy(active = fin != semi.zero, finalWeight = fin)
        }
      }
      case Alternative(p, q) => {
        Components.alternative(shift(mark, p, c), shift(mark, q, c))
      }
      case Sequence(p, q) => {
        Components.sequence(shift(mark, p, c), shift(semi.add(semi.multiply(mark, p.get.emptyWeight), p.get.finalWeight), q, c))
      }
      case Repetition(r) => {
        Components.repetition(shift(semi.add(mark, r.get.finalWeight), r, c))
      }
    }
  }

  private def shift[C, S](mark: S, state: Lazy[State[C, S]], c: C)(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    val value = state.get
    if (value.active || (mark != semi.zero)) {
      step(mark, value.component, c)
    } else {
      state
    }
  }
}
