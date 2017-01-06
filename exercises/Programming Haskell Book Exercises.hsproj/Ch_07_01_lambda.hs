module Lamb where
    
addOneIfOdd n = case odd n of 
    True -> f n
    False -> n
    where f = \n -> n + 1
    


-- p. 355 pattern matching    
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f)) 
    
-- p. 360 case expressions
functionC x y = if (x > y) then x else y


funcC :: Ord a => a -> a -> a 
funcC x y = 
    case (x > y) of
        True -> x
        False -> y
        
        
ifEvenAdd2 n = if even n then (n+2) else n

ifEA2 :: Integral a => a -> a
ifEA2 n = 
    case (even n) of
        True -> n + 2
        False -> n
    
nums :: (Num a, Ord a) => a -> a
nums x =
    case compare x 0 of
        LT -> -1 
        GT -> 1
        EQ -> 0
        
        
-- p. 372 higher order functions
dodgy :: Num a => a -> a -> a 
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- p. 381 Guard Duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.6 = 'D'
    | y >= 0.5 = 'E'
    | y <  0.5 = 'F'
    where y = x / 100

pal :: Eq a => [a] -> Bool    
pal xs 
    | xs == reverse xs = True
    | otherwise = False

numbers :: (Ord a, Num a) => a -> a
numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1


