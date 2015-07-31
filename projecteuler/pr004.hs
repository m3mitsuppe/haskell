-- Project Euler problem 4: Largest palindrome product
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 
-- 2-digit numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List (sort)

is_palindrom :: (Eq a) => [a] -> Bool
is_palindrom [e] = True
is_palindrom (x:y:[]) = x == y
is_palindrom (x:xs) = if x == (last xs)
					  then is_palindrom (init xs)
					  else False

get_palindrom_products :: (Integral a, Show a) => a -> [(a,a,a)]
get_palindrom_products start = check_pal start start [] where
    check_pal 1 _ result = result
    check_pal x y xs = if is_palindrom (show (x * y))
                       then if x == y
                           then check_pal (x-1) start [(x * y, x, y)] ++ xs
                           else check_pal x (y-1) [(x * y, x, y)] ++ xs
                       else if x == y
                           then check_pal (x-1) start xs
                           else check_pal x (y-1) xs

largest_pp :: (Integral a, Show a) => a -> (a,a,a)
largest_pp = maximum . get_palindrom_products                       
                           
{-Algorithmus:  Starte mit 999 / 999
                Erniedrige den ersten Wert
                998 / 999
                Erniedrige solange den zweiten Wert, bis a==b
                Dann erniedrige den ersten Wert und fange beim zweiten Wert 
                wieder oben an
                etc.
                    -}