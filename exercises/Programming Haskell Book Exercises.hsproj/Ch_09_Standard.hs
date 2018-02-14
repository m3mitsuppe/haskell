module Ch_09_Standard where
  
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
          | e == x = True
          | otherwise = myElem e xs
          

-- Alternativlösung mit "any"
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e es = myAny ((==) e) es

