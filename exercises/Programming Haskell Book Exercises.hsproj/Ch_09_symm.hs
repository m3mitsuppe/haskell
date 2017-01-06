module Ch_09_symm where
  
myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords s = takeWhile (/=' ') s : myWords (dropWhile (/=' ') s)


mySplitAt :: Char -> String -> [String]
mySplitAt _ [] = []
mySplitAt sp text
            | sp == head text = mySplitAt sp (tail text)
            | otherwise = takeWhile (/=sp) text : mySplitAt sp (dropWhile (/=sp) text)
          
mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]