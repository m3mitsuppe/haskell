module DigitalRoot where

digitalRoot :: Integral a => a -> a
digitalRoot x
     | x < 10 = x
     | otherwise = digitalRoot(digitalRoot (x `div` 10) + (x `mod` 10))