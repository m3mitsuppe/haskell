module Polymorphism where
  
import Data.List (sort)
  
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah | Woot deriving (Show, Eq)

-- settleDown :: Mood -> Mood 

settleDown x = if x == Woot 
               then Blah
               else x


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "allover"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p

-- f :: Float
f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: a -> Int
sigmund x = myX

-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

chk :: Eq b => (a -> b) a -> b -> Bool
chk fnc x y = 




