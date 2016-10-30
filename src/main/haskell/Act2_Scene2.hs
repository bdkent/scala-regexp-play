submatchw :: Semiring s ⇒ REGw (Int, c) s → [c] → s
submatchw r s =
  matchw (seqw arb (seqw r arb)) (zip [0..] s)
  where arb = repw (symw (λ_ → one))

class Semiring s ⇒ Semiringi s where
index :: Int → s

symi :: Semiringi s ⇒ Char → REGw (Int, Char) s
symi c = symw weight
  where weight (pos, x) | x == c    = index pos
                        | otherwise = zero

data Leftmost = NoLeft | Leftmost Start

data Start = NoStart | Start Int

instance Semiring Leftmost where
zero = NoLeft
one = Leftmost NoStart

NoLeft     ⊕ x          = x
x          ⊕ NoLeft     = x
Leftmost x ⊕ Leftmost y = Leftmost (leftmost x y)
  where leftmost NoStart NoStart = NoStart
        leftmost NoStart (Start i) = Start i
        leftmost (Start i) NoStart = Start i
        leftmost (Start i) (Start j) = Start (min i j)  

NoLeft     ⊗ _          = NoLeft
_          ⊗ NoLeft     = NoLeft
Leftmost x ⊗ Leftmost y = Leftmost (start x y)
  where start NoStart s = s
        start s       _ = s

instance Semiringi Leftmost where
index = Leftmost · Start

data LeftLong = NoLeftLong | LeftLong Range

data Range = NoRange | Range Int Int

LeftLong x ⊕ LeftLong y = LeftLong (leftlong x y)
where leftlong ...
      leftlong (Range i j) (Range k l)
        | i < k ∨ i == k ∧ j >= l = Range i j
        | otherwise               = Range k l

LeftLong x ⊗ LeftLong y = LeftLong (range x y)
  where range ...
        range (Range i _ ) (Range _ j) = Range i j

instance Semiringi LeftLong where
  index i = LeftLong (Range i i)
  