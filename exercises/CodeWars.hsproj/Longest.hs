module Longest where
  
import Data.List (sort)

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = r2 (sort $ s1 ++ s2) 'a'
    where 
      r2 :: [Char] -> Char -> [Char]
      r2 []  _  = []
      r2 (a:ass) cmp
          | a == cmp = a : r2 ass (succ cmp)
          | a < cmp = r2 ass cmp
          | a > cmp = a : r2 ass (succ a)