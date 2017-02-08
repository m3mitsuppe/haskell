module Ch_09_zip where
  
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a, b) : myzip as bs

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]

myzipwith _ [] _ = []
myzipwith _ _ [] = []
myzipwith f (a:as) (b:bs) = f a b : myzipwith f as bs

mz :: [a] -> [b] -> [(a,b)]
mz x y = myzipwith (,) x y