-- Problem 10 - Summation of primes
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

isPrime :: (Integral a) => a -> Bool    
isPrime x =
    notElem True [x `mod` y == 0 | y <- [2.. round(sqrt (fromIntegral x))]]

getPrimes :: (Integral a) => a -> [a]
getPrimes max = [ x | x <- [2..max], isPrime x]

sumPrimes :: (Integral a) => a -> a
sumPrimes max = sum $ getPrimes max