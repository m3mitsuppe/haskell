module DividedBy where
    
import Data.List (intersperse)

data DividedResult = Result (Integer, Integer)
                     | DividedByZero 
                     deriving Show
    
dividedBy :: Integer -> Integer -> DividedResult

dividedBy 0 _ = Result (0, 0)
dividedBy _ 0 = DividedByZero

dividedBy num denom = go num denom 0
    where fac = if (num * denom) > 0 then 1 else (-1)
          go n d count
            | abs n < abs d = Result (fac*count, n)
            | otherwise = go (abs n - abs d) d (count+1)

mc91 :: Integer -> Integer
mc91 x
       | x > 100 = x -10
       | otherwise = mc91 . mc91 $ x + 11
       
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7  = "seven"
digitToWord 8  = "eight"
digitToWord 9  = "nine"
digitToWord _  = "Oh come on!"

digits :: Int -> [Int]
digits x
        | d < 10 = [d, m]
        | otherwise = (digits d) ++ [m]
        where
             (d, m) = divMod x 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits


{-
Funktioniert nicht:
flatten :: [[a]] -> [a]
flatten [] = []
flatten ([]:xs) = flatten xs
flatten ([x]:xs) = x : flatten xs
-}
--flatten ((x : (y : [ys])) : z) = x : y : ys : flatten z


