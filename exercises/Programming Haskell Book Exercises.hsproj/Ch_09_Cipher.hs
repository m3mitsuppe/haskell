module Ch_09_Cipher where
  
import Data.Char
            
char_ranges = [(ord a, ord z) | (a, z) <- [('A','Z'),('a','z')]]
non_alpha = ",;.:-_!? "
data Direction = Up | Down

ceasar :: String -> Int -> String
ceasar [] _ = []
ceasar (x:xs) n = (shift char_ranges x n Up) : ceasar xs n

unceasar :: String -> Int -> String
unceasar [] _ = []
unceasar (x:xs) n = (shift char_ranges x n Down) : unceasar xs n

shift :: [(Int, Int)] -> Char -> Int -> Direction -> Char
shift ((a,z):ranges) c i dir
            | c `elem` non_alpha = c
            | ord c >= a && ord c <= z = do_shift (a,z) c i dir
            | otherwise = shift ranges c i dir


do_shift :: (Int, Int) -> Char -> Int -> Direction -> Char
do_shift (a,z) c i Up
            | ord c <= z - i = chr ((ord c) + i)
            | otherwise = chr ((ord c) - 26 + i)
do_shift (a,z) c i Down
            | ord c >= a + i = chr ((ord c) - i)
            | otherwise = chr ((ord c) + 26 - i)

