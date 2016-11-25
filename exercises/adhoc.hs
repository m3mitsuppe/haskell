module Adhoc where
    
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = b == (aTob a)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aTob int a = (aTob a) + fromInteger int