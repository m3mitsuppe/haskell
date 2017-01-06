module Ch_09_enum where

eftBool :: Bool -> Bool -> [Bool]
eftBool s e 
      | s > e = []
      | s == e = [s]
      | otherwise = [s, e]
      
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd s e
      | s > e = []
      | s == e = [e]
      | otherwise = s : eftOrd (succ s) e
      
eftInt :: Int -> Int -> [Int]
eftInt s e
      | s > e = []
      | s == e = [e]
      | otherwise = s : eftInt (succ s) e
      

-- Verallgemeinerung der obigen Lösung
eft :: (Ord a, Enum a) => a -> a -> [a]
eft s e
      | s > e = []
      | s == e = [e]
      | otherwise = s : eft (succ s) e