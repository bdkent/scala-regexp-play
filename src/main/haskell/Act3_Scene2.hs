-- added a boolean field active to the data type REGw

seqw :: Semiring s ⇒ REGw c s → REGw c s → REGw c s
seqw p q =
  REGw { active = active p ∨ active q,
         emptyw = emptyw p ⊗ emptyw q,
         finalw = finala p ⊗ emptyw q ⊕ finala q,
         regw = SEQw p q }

seq p q = REGw { active = False,
                 emptyw = emptyw p ⊗ emptyw q,
                 finalw = zero,
                 regw = SEQw p q }

-- same for altw, repw

finala ::Semiring s ⇒ REGw c s → s
finala r = if active r then finalw r else zero

-- shiftw change
shiftw :: Semiring s ⇒ s → REw c s → c → REGw c s
shiftw ...
shiftw m (SYMw f)  c =
  let fin = m ⊗ f c
  in (symw f) {active = fin = ̸= zero, finalw = fin}

shiftw :: (Eq s, Semiring s) ⇒ s → REGw c s → c → REGw c s
shiftw m r c | active r ∨ m =/= zero = stepw m (regw r) c
             | otherwise            = r

-- rename shiftw to stepw with recursive calls to shiftw
