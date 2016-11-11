module EQ_instances where
    
data TisAnInteger = TisAn Integer deriving Show

instance Eq (TisAnInteger) where
    (==) (TisAn v) (TisAn v') = v == v'
    

data TwoIntegers = Two Integer Integer

instance Eq (TwoIntegers) where
    (==) (Two x y) (Two x' y') = x == x' && y == y'


data StringOrInt = TisAnInt Int | TisAString String

instance Eq (StringOrInt) where
    TisAnInt i   == TisAnInt i'   = i == i'
    TisAString t == TisAString t' = t == t'
    TisAnInt _ == TisAString _ = False
    TisAString _ == TisAnInt _ = False
    

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair v w) (Pair v' w') = v == v' && w == w'
    
    
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple v w) (Tuple v' w') = v == v' && w == w'
    
    
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne v) (ThisOne v') = v == v'
    (==) (ThatOne v) (ThatOne v') = v == v'
    (==) (ThatOne _) (ThisOne _) = False
    (==) (ThisOne _) (ThatOne _) = False
    

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello v)   (Hello v')   = v == v'
    (==) (Goodbye v) (Goodbye v') = v == v'
    (==) (Hello _)   (Goodbye _)  = False
    (==) (Goodbye _) (Hello _)    = False