module Flippy where
    
-- p. 444 currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Hier sind Guards vermutlich sinnvoll, weil die Bedingung
-- "n == a" nicht durch Pattern Matching abgebildet werden kann.
-- "where go a a = a" ist nicht erlaubt.
recSum :: (Eq a, Num a) => a -> a
recSum x = go 1 x
    where go n a
            | n == a = a
            | otherwise = n + go (n+1) a

-- Variante: runterzählen statt hoch
recSum3 :: (Eq a, Num a) => a -> a
recSum3 1 = 1
recSum3 x = x + recSum2 (x-1)


-- Warum Guards? Machen hier eigentlich keinen Sinn, weil
-- Pattern Matching genutzt werden kann (s. recMult4)
-- Annahme: Guards nutzen, wenn eben kein Pattern Matching 
-- möglich ist, sondern die Bedingung komplexer ist
-- Erster Versuch mit "go"-Funktion: ist hier unnötig, weil
-- keine extra Zählvariable gebrauht wird
recMult :: Integral a => a -> a -> a
recMult x y = go x y
    where go c b
             | c == 1 = b
             | otherwise = b + go (c-1) b
             
-- Variante ohne Zählvariable, aber mit Guards
recMult2 :: Integral a => a -> a -> a
recMult2 x y 
    | x == 1 = y
    | otherwise = y + recMult2 (x-1) y
    
-- Variante mit Pattern Matching - klarstes Ergebnis
recMult3 :: Integral a => a -> a -> a
recMult3 1 y = y
recMult3 x y = y + recMult3 (x-1) y

-- Versuch: Pattern Matching bei einer als "where-clause" 
-- definierten Funktion. Funktioniert!
recMult4 :: Integral a => a -> a -> a
recMult4 x y = go x y
    where go 1 b = b
          go a b = b + go (a-1) b