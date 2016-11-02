package regexp.api

class Lazy[+A](a: => A) {
  def get: A = a
}

object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy(a)
}

case class State[+C, +S](active: Boolean = false, emptyWeight: S, finalWeight: S, State: Component[C, S])

sealed trait Component[+C, +S]
case object Empty extends Component[Nothing, Nothing]
case class Symbol[C, +S](f: (C => S)) extends Component[C, S]
case class Alternative[+C, +S](p: Lazy[State[C, S]], q: Lazy[State[C, S]]) extends Component[C, S]
case class Sequence[+C, +S](p: Lazy[State[C, S]], q: Lazy[State[C, S]]) extends Component[C, S]
case class Repetition[+C, +S](r: Lazy[State[C, S]]) extends Component[C, S]

private[api] object Components {
  def empty[C, S]()(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        emptyWeight = semi.one,
        finalWeight = semi.zero,
        State = Empty)
    }
  }

  def symbol[C, S](f: (C => S))(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        emptyWeight = semi.zero,
        finalWeight = semi.zero,
        State = Symbol(f))
    }
  }

  def alternative[C, S](p: Lazy[State[C, S]], q: => Lazy[State[C, S]])(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        active = p.get.active || q.get.active,
        emptyWeight = semi.add(p.get.emptyWeight, q.get.emptyWeight),
        finalWeight = semi.add(determineFinal(p.get), determineFinal(q.get)),
        State = Alternative(p, q))
    }
  }

  def sequence[C, S](p: => Lazy[State[C, S]], q: => Lazy[State[C, S]])(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        active = p.get.active || q.get.active,
        emptyWeight = semi.multiply(p.get.emptyWeight, q.get.emptyWeight),
        finalWeight = semi.add(semi.multiply(determineFinal(p.get), q.get.emptyWeight), determineFinal(q.get)),
        State = Sequence(p, q))
    }
  }

  def repetition[C, S](r: => Lazy[State[C, S]])(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        active = r.get.active,
        emptyWeight = semi.one,
        finalWeight = determineFinal(r.get),
        State = Repetition(r))
    }
  }

  private def determineFinal[C, S](r: State[C, S])(implicit semi: Semiring[S]): S = {
    if (r.active) r.finalWeight else semi.zero
  }

}

object Builder {
  def empty[C, S]()(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Components.empty()
  }

  def symbol[C, S](f: (C => S))(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Components.symbol(f)
  }

  def sequence[C, S](p: => Lazy[State[C, S]], q: => Lazy[State[C, S]])(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        emptyWeight = semi.multiply(p.get.emptyWeight, q.get.emptyWeight),
        finalWeight = semi.zero,
        State = Sequence(p, q))
    }
  }

  def alternative[C, S](p: => Lazy[State[C, S]], q: => Lazy[State[C, S]])(implicit semi: Semiring[S]): Lazy[State[C, S]] = {
    Lazy {
      State(
        emptyWeight = semi.add(p.get.emptyWeight, q.get.emptyWeight),
        finalWeight = semi.zero,
        State = Alternative(p, q))
    }
  }
}
