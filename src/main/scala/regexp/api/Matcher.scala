package regexp.api

object StringMatcher {
  def run[S](r: State[Char, S], str: String)(implicit semi: Semiring[S]): S = {
    Matcher.run[Char, S](r, str.toList)
  }

  def submatch[S](r: State[(Char, Int), S], str: String)(implicit semi: Semiring[S]): S = {
    Matcher.submatch[Char, S](r, str.toList)
  }

  def accept[S](r: State[Char, S], str: String)(implicit semi: Semiring[S]): S = {
    Validator.accept(r, str.toList)
  }
}

object Matcher {

  def run[C, S](state: State[C, S], xs: List[C])(implicit semi: Semiring[S]): S = {
    //    println(s"run(_, $xs)")
    xs match {
      case Nil => {
        state.emptyWeight.get
      }
      case c :: cs => {
        cs.foldLeft(step(semi.one, state.component.get, c))((x, y) => step(semi.zero, x.component.get, y)).finalWeight.get
      }
    }
  }

  def submatch[C, S](r: State[(C, Int), S], s: List[C])(implicit semi: Semiring[S]): S = {
    def f(x: (Int, C)) = semi.one
    lazy val arb = Components.repetition(Components.symbol(f))
    run(Components.sequence(arb, Components.sequence(r, arb)), s.zipWithIndex)
  }

  private def step[C, S](mark: S, component: Component[C, S], c: C)(implicit semi: Semiring[S]): State[C, S] = {
    //    println(s"step($mark, $component, $c)")
    component match {
      case Empty => Components.empty()
      case Symbol(f) => {
        val f1: (C => S) = f.asInstanceOf[(C => S)] // YUCK
        lazy val fin = semi.multiply(mark, f1(c))
        Components.symbol(f1).copy(active = Lazy(fin != semi.zero), finalWeight = Lazy(fin))
      }
      case Alternative(p, q) => {
        Components.alternative(shift(mark, p.get, c), shift(mark, q.get, c))
      }
      case Sequence(p, q) => {
        Components.sequence(shift(mark, p.get, c), shift(semi.add(semi.multiply(mark, p.get.emptyWeight.get), p.get.finalWeight.get), q.get, c))
      }
      case Repetition(r) => {
        Components.repetition(shift(semi.add(mark, r.get.finalWeight.get), r.get, c))
      }
    }
  }

  private def shift[C, S](mark: S, state: State[C, S], c: C)(implicit semi: Semiring[S]): State[C, S] = {
    //    println(s"shift($mark, _, $c)")
    val value = state
    if (value.active.get || (mark != semi.zero)) {
      step(mark, value.component.get, c)
    } else {
      state
    }
  }
}

