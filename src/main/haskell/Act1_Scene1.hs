data Reg = Eps            -- ε
         | Sym Char       -- α
         | Alt Reg Reg	   -- α|β
         | Seq Reg Reg    -- αβ
         | Rep Reg        -- α∗

accept :: Reg → String → Bool
accept Eps u = null u
accept (Sym c) u = u == [ c ]
accept (Alt p q) u = accept p u \/ accept q u
accept (Seq p q) u =
  or [accept p u1 /\ accept q u2 | (u1,u2) ← split u]
accept (Rep r) u =
  or[and[accept r ui | ui ← ps] | ps ← parts u]

split :: [a] → [([a], [a])]
split [] = [([], [])]
split (c : cs) = ([], c : cs) : [(c : s1,s2) | (s1,s2) ← split cs]

parts :: [a] → [[[a]]]
parts [] = [[]]
parts [c] = [[[c]]]
parts (c : cs) =
  concat [[(c:p):ps,[c]:p:ps] | p:ps ← parts cs]