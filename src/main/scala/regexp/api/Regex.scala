package regexp.api

class Lazy[+A](a: => A) {
  def get: A = a
}

object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy(a)
}

case class State[+C, +S](active: Lazy[Boolean] = Lazy(false), emptyWeight: Lazy[S], finalWeight: Lazy[S], component: Lazy[Component[C, S]])

sealed trait Component[+C, +S]
case object Empty extends Component[Nothing, Nothing]
case class Symbol[C, S](f: (C => S)) extends Component[C, S]
case class Alternative[+C, S](p: Lazy[State[C, S]], q: Lazy[State[C, S]]) extends Component[C, S]
case class Sequence[+C, S](p: Lazy[State[C, S]], q: Lazy[State[C, S]]) extends Component[C, S]
case class Repetition[+C, S](r: Lazy[State[C, S]]) extends Component[C, S]

private[api] object Components {

  def empty[C, S]()(implicit semi: Semiring[S]): State[C, S] = {
    State(
      emptyWeight = Lazy(semi.one),
      finalWeight = Lazy(semi.zero),
      component = Lazy(Empty))
  }

  def symbol[C, S](f: (C => S))(implicit semi: Semiring[S]): State[C, S] = {
    State(
      emptyWeight = Lazy(semi.zero),
      finalWeight = Lazy(semi.zero),
      component = Lazy(Symbol(f)))
  }

  def alternative[C, S](p: State[C, S], q: => State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      active = Lazy(p.active.get || q.active.get),
      emptyWeight = Lazy(semi.add(p.emptyWeight.get, q.emptyWeight.get)),
      finalWeight = Lazy(semi.add(determineFinal(p), determineFinal(q))),
      component = Lazy(Alternative(Lazy(p), Lazy(q))))
  }

  def sequence[C, S](p: State[C, S], q: => State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      active = Lazy(p.active.get || q.active.get),
      emptyWeight = Lazy(semi.multiply(p.emptyWeight.get, q.emptyWeight.get)),
      finalWeight = Lazy(semi.add(semi.multiply(determineFinal(p), q.emptyWeight.get), determineFinal(q))),
      component = Lazy(Sequence(Lazy(p), Lazy(q))))
  }

  def repetition[C, S](r: State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      active = r.active,
      emptyWeight = Lazy(semi.one),
      finalWeight = Lazy(determineFinal(r)),
      component = Lazy(Repetition(Lazy(r))))
  }

  def determineFinal[C, S](r: State[C, S])(implicit semi: Semiring[S]): S = {
    if (r.active.get) r.finalWeight.get else semi.zero
  }

}

object Builder {
  def empty[C, S]()(implicit semi: Semiring[S]): State[C, S] = {
    Components.empty()
  }

  def symbol[C, S](f: (C => S))(implicit semi: Semiring[S]): State[C, S] = {
    Components.symbol(f)
  }

  def symbolIs[C](c: C)(implicit semi: Semiring[Boolean]): State[C, Boolean] = {
    def f(x: Char): Boolean = { x == c }
    symbol(f _)
  }

  def symbol[C, S](c: C)(implicit semi: IndexedSemiring[S]): State[(C, Int), S] = {
    def f(t: (C, Int)): S = {
      t match {
        case (x, pos) if x == c => semi.index(pos)
        case _                  => semi.zero
      }
    }
    symbol(f _)
  }

  def symbolZero[S](c: Char)(implicit semi: Semiring[S]): State[Char, S] = {
    def f(x: Char): S = {
      if (x == c) semi.one else semi.zero
    }
    symbol(f _)
  }

  def sequence[C, S](p: State[C, S], q: => State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      emptyWeight = Lazy(semi.multiply(p.emptyWeight.get, q.emptyWeight.get)),
      finalWeight = Lazy(semi.zero),
      component = Lazy(Sequence(Lazy(p), Lazy(q))))
  }

  def alternative[C, S](p: State[C, S], q: => State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      emptyWeight = Lazy(semi.add(p.emptyWeight.get, q.emptyWeight.get)),
      finalWeight = Lazy(semi.zero),
      component = Lazy(Alternative(Lazy(p), Lazy(q))))
  }

  def repetition[C, S](r: State[C, S])(implicit semi: Semiring[S]): State[C, S] = {
    State(
      emptyWeight = Lazy(semi.one),
      finalWeight = Lazy(semi.zero),
      component = Lazy(Repetition(Lazy(r))))
  }
}
