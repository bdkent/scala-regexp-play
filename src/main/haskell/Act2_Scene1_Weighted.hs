data REGw c s = REGw { emptyw :: s,
                       finalw :: s,
                       regw   :: REw c s }

data REw c s = EPSw
             | SYMw (c → s)
             | ALTw (REGw c s)(REGw c s)
             | SEQw (REGw c s)(REGw c s)
             | REPw (REGw c s)

epsw :: Semiring s ⇒ REGw c s
epsw = REGw { emptyw = one,
              finalw = zero,
              regw   = EPSw }

symw :: Semiring s ⇒ (c → s) → REGw c s
symw f = REGw { emptyw = zero,
                finalw = zero,
                regw   = SYMw f }

altw :: Semiring s ⇒ REGw c s → REGw c s → REGw c s
altw p q = REGw { emptyw = emptyw p ⊕ emptyw q,
                  finalw = finalw p ⊕ finalw q,
                  regw   = ALTw p q }

seqw ::Semiring s ⇒ REGw c s → REGw c s → REGw c s
seqw p q =
  REGw { emptyw = emptyw p ⊗ emptyw q,
         finalw = finalw p ⊗ emptyw q ⊕ finalw q,
         regw = SEQw p q }

repw :: Semirings ⇒ REGw c s → REGw c s
repw r = REGw { emptyw = one,
                finalw = finalw r,
                regw   = REPw r }

matchw :: Semiring s ⇒ REGw c s → [c] → s
matchw r [ ]      = emptyw r
matchw r (c : cs) =
 finalw (foldl (shiftw zero · regw) (shiftw one (regw r) c) cs)

shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
shiftw _  EPSw      _ = epsw
shiftw m (SYMw f)   c = (symw f) {finalw = m ⊗ f c}
shiftw m (ALTw p q) c =
  altw (shiftw m (regw p) c) (shiftw m (regw q) c)
shiftw m (SEQw p q) c =
  seqw (shiftw m (regw p) c)
       (shiftw (m ⊗ emptyw p ⊕ finalw p) (regw q) c)
shiftw m (REPw r)   c =
  repw (shiftw (m ⊕ finalw r) (regw r) c)