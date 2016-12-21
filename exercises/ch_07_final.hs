-- p. 398 exercises

module Code where
    
tensDigit :: Integral a => a -> a 
tensDigit x = d
    where (y, _) = divMod x 10
          (_, d) = divMod y 10

hunsDigit :: Integral a => a -> a 
hunsDigit x = d
    where (y, _) = divMod x 100
          (_, d) = divMod y 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
        True -> x
        False -> y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
    | b == True = x
    | otherwise = y
    
    
g :: (a -> b) -> (a, c) -> (b, c)
g fn (a, c) = (fn a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

main = do
    print ((roundTrip 4) :: Integer)
    print (id 4)
    
data SumOfTwo a b = FirstP a 
                  | SecondP b  
                  deriving (Eq, Show)