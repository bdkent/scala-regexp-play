data REG = EPS
         | SYM Bool Char
         | ALT REG REG
         | SEQ REG REG
         | REP REG

shift :: Bool → REG → Char → REG
shift _  EPS      _ = EPS
shift m (SYM _ x) c = SYM (m ∧ x == c) x
shift m (ALT p q) c = ALT (shift m p c) (shift m q c)
shift m (SEQ p q) c =
  SEQ (shift m p c)
      (shift (m ∧ empty p ∨ final p) q c)
shift m (REP r) c = REP (shift (m ∨ final r) r c)

empty :: REG → Bool
empty EPS       = True
empty (SYM _ _) = False
empty (ALT p q) = empty p ∨ empty q
empty (SEQ p q) = empty p ∧ empty q
empty (REP r)   = True

final :: REG → Bool
final  EPS      = False
final (SYM b _) = b
final (ALT p q) = final p ∨ final q
final (SEQ p q) = final p ∧ empty q ∨ final q
final (REP r)   = final r


match :: REG → String → Bool
match r [ ] = empty r
match r (c:cs) =
  final (foldl (shift False) (shift True r c) cs)