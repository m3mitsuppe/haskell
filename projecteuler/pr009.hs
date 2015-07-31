{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

isPythTriple :: (Integral a) => a -> a -> a -> Bool
isPythTriple a b c = a^2 + b^2 == c^2

findTriple1000 :: Integer
findTriple1000 = find 1 1 where
	find 998 two = find 1 (two+1)
	find one two = if isPythTriple one two three
				   then one*two*three
				   else find (one+1) two
				   where three = 1000 - one - two		
