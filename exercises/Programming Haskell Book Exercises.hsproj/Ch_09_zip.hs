module Ch_09_zip where
  
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a, b) : myzip as bs