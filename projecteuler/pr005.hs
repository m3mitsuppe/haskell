-- Project Euler problem 5: Smallest number evenly divided by all numbers from 1 to 20
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What ist the smallest positive number that is evenly divisible by all the numbers from 1 to 20?

{-
Lösungsidee: 
- Primzahlen von 1 bis 20 multiplizieren
- Start = Resultat - (Resultat mod 20) - auf 20er abrunden
- Dann in 20er-Schritten
- Der Reihe nach den Rest ermitteln bei Division durch: 
   - 19, 18, 17, 16, 15, 14, 13, 12, 11 (alle anderen sind durch die größeren Zahlen erledigt)
   - Hilfsfunktion dafür
-}

isPrime :: (Integral a) => a -> Bool    
isPrime x =
    notElem True [x `mod` y == 0 | y <- [2.. round(sqrt (fromIntegral x))]]
    
getPrimes :: (Integral a) => a -> [a]
getPrimes max =
    [ x | x <- [2..max], isPrime x]



divisibleBy :: (Integral a) => a -> [a] -> Bool
divisibleBy _ [] = True
divisibleBy num (x:xs) = if num `mod` x == 0 
							then divisibleBy num xs
						    else False

startValue :: (Integral a) => a -> a
startValue base = p - (p `mod` base) where
	p = product . getPrimes $ base

divisors10 = [9,8,7,6]
divisors20 = [19,18,17,16,15,14,13,12,11]

smallestMultiple :: (Integral a) => a -> [a] -> a
smallestMultiple num divisors = checkMultiples (startValue num) where
	checkMultiples current = if divisibleBy current divisors
		                          then current
		                          else checkMultiples (current + num)	


