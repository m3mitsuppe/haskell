module LetWhere where

ex1let = let x = 3
             y = 1000
         in x * y + 3
         
ex1where = x * y + 3 where
    x = 3
    y = 1000
    
ex2let = let y = 10
             x = 10 * 5 + y
         in x * 5
         
ex2where = x * 5 where
    y = 10
    x = 10 * 5 + y
    
ex3let = let x = 7
             y = negate x
             z = y * 10
         in z / x + y
         
ex3where = z / x + y where
    x = 7
    y = negate x
    z = y * 10
    
-- test = print ex1let == ex1where
