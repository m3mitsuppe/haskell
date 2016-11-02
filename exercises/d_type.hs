{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
    
-- f1 :: Num a => a
f1 = (* 9) 6

-- f2 :: Num a => (a, [Char]) 
f2 = head [(0,"doge"),(1,"kitteh")]

-- f3 :: (Integer, [Char]) -- keine Typeclass Num,
--                            weil Integer Num enthält
f3 = head [(0 :: Integer ,"doge"),(1,"kitteh")]

-- f4 :: Bool
f4 = if False then True else False

-- f5 :: Int
f5 = length [1, 2, 3, 4, 5]

-- f6 :: Bool
f6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- w :: Num t => t
x = 5
y = x + 5
w = y * 10

-- ---------------

-- z1 :: Num t => t -> t
x1 = 5
y1 = x1 + 5
z1 y1 = y1 * 10

--
-- f :: Fractional a => a
x2 = 5
y2 = x2 + 5
f7 = 4 / y2

--
-- f8 :: [Char]
x3 = "Julie"
y3 = " <3 "
z3 = "Haskell"
f8 = x3 ++ y3 ++ z3

bigNum x = (^) 5 $ x
wahoo = bigNum $ 10

-- functionC :: Ord a => a -> a -> Bool
-- functionS :: (a, b) -> b

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = tail x -- alle ein-argumentigen Listenfunktionen,
--              z.B. auch reverse, etc

-- Only one version that will typecheck
co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob a = bToc (aTob a)

fa :: (a -> c) -> a -> a
fa _ a = a

fa' :: (a -> b) -> a -> b
fa' aTob a = aTob a