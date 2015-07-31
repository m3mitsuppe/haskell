-- Project Euler problem 3: Largest prime factor

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

import Data.List (sort)

getSqrt :: (Integral a) => a -> a
getSqrt = ceiling . sqrt . fromIntegral

isPrime :: (Integral a) => a -> Bool    
isPrime x =
    notElem True [x `mod` y == 0 | y <- [2.. getSqrt x]]

largestPrime :: (Integral a) => a -> a
largestPrime x = checkDivisor x (x `div` 2) where
    checkDivisor x t = if x `mod` t == 0 && isPrime t
        then t
        else checkDivisor x (t-1)

isSquare :: (Integral a) => a -> Bool
isSquare x = x == r*r where
    r = getSqrt x
    
factorize :: (Integral a) => a -> [a]
factorize n = case factorizeFermat n of
    (a,1) -> [a]
    (a,b) -> sort (factorize a ++ factorize b)           
    
-- findet nicht unbedingt die Primfaktoren, 
-- muss daher ggf. wiederholt angewandt werden   
factorizeFermat :: (Integral a) => a -> (a,a)    
factorizeFermat n = 
    let x = getSqrt(n)
        r = x*x - n
    in getFactors x r
            
getFactors :: (Integral a) => a -> a -> (a,a)            
getFactors x r 
    | isSquare r = let y = getSqrt r
                       a = x + y
                       b = x - y
                   in (a,b)
    | otherwise = let r' = r + 2*x + 1
                      x' = x + 1
                  in getFactors x' r'    
           
            
            
            
            
            