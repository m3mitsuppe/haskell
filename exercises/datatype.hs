module DataType where
    
data Mood = Blah | Woot deriving Show

rvmd :: Mood -> Mood
rvmd Blah = Woot
rvmd _    = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > (-1) then x else x * (-1)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x = (+)
f1 xs = w `x` 1
        where w = length xs

