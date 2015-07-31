-- 10001st prime
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
-- we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

isPrime :: (Integral a) => a -> Bool    
isPrime x =
    notElem True [x `mod` y == 0 | y <- [2.. round(sqrt (fromIntegral x))]]

getPrimes :: (Integral a) => a -> [a]
getPrimes max =
    [ x | x <- [2..max], isPrime x]

solve7 :: Integer
solve7 = check 1 2 where
	check c p = if isPrime p
				then if c == 10001
					 then p
					 else check (c+1) (p+1)
				else check c (p+1)

solve7b :: Integer
solve7b = getPrimes 200000 !! 10000