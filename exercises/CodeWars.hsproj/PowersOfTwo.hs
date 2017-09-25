module PowersOfTwo where

powersOfTwo :: Int -> [Int]
powersOfTwo n = reverse $ pOT n where
      pOT 0 = [1]
      pOT n = 2^n : pOT (n-1)

pOT2 :: Int -> [Int]
pOT2 n = map (2^) [0..n] 