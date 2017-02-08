module Ch_09_Cipher where
  
import Data.Char

ceasar :: String -> Int -> String
ceasar [] _ = []
ceasar (x:xs) n = (shift x n) : ceasar xs n

unceasar :: String -> Int -> String
unceasar [] _ = []
unceasar (x:xs) n = (shift x (-n)) : unceasar xs n 


shift :: Char -> Int -> Char
shift c i
      | c == ' ' = ' '
      | c `elem` ['A'..'Z'] = shiftByFromTo c ('A','Z') i 
      | c `elem` ['a'..'z'] = shiftByFromTo c ('a','z') i
      | otherwise = ' '
        

shiftByFromTo :: Char -> (Char, Char) -> Int -> Char
shiftByFromTo c (a, z) i
            | ord c <= ord z - i = chr ((ord c) + i)
            | otherwise = chr ((ord c) - 26 + i)  
            

shift2 :: Char -> (Char, Char) -> Int -> Maybe Char
shift2 c (a, z) i
            | shifted `elem` [a..z] = Just shifted
            | shifted < a = Just ' '
            where 
              shifted = chr ((ord c) + i)