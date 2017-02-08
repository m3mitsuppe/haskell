module Ch_09_Upper where
  
import Data.Char

onlyUpper :: String -> String
onlyUpper [] = []
onlyUpper (x:xs)
          | isUpper x = x : onlyUpper xs
          | otherwise = onlyUpper xs
          
capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = toUpper x : xs

capAll :: String -> String
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

getCapFirst :: String -> Char
getCapFirst = toUpper . head