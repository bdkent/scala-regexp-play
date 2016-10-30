class Semiring s where
  zero, one :: s
  (⊕), (⊗) :: s → s → s

instance Semiring Bool where zero = False
  one = True
  (⊕) = (∨)
  (⊗) = (∧)

data Regw c s = Epsw
              | Symw (c → s)
              | Altw (Regw c s) (Regw c s)
              | Seqw (Regw c s) (Regw c s)
              | Repw (Regw c s)

instance Semiring Int where
zero = 0
one = 1
(⊕) = (+)
(⊗) = (∗)

sym :: Semiring s ⇒ Char → Regw Char s
sym c = Symw (λx → if x==c then one else zero)

weighted :: Semiring s ⇒ Reg → Regw Char s
weighted Eps = Epsw
weighted (Sym c) = sym c
weighted (Alt p q) = Altw (weighted p) (weighted q)
weighted (Seq p q) = Seqw (weighted p) (weighted q)
weighted (Rep p) = Repw (weighted p)

acceptw ::Semiring s ⇒ Regw c s → [c] → s
acceptw Epsw u = if null u then one else zero
acceptw (Symw f) u = case u of [c] → f c; _ → zero
acceptw (Altw p q) u = acceptw p u ⊕ acceptw q u
acceptw (Seqw p q) u =
  sum [acceptw p u1 ⊗ acceptw q u2 | (u1, u2) ← split u]
acceptw (Repw r) u =
  sum [prod [acceptw r ui | ui ← ps] | ps ← parts u]


sum, prod :: Semiring s ⇒ [s] → s
sum = foldr (⊕) zero
prod = foldr (⊗) one

