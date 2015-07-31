-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
-- The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

pr001 = sum3s + sum5s where
        sum3s = sum [3,6..999]
        sum5s = sum [ x | x <- [5,10..999], x `mod` 3 /= 0] 
        
        