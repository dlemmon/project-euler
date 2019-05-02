{-
euler 63
-------




The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.

How many n-digit positive integers exist which are also an nth power?



-}

import Data.List


findPower exp = [x | x <- [1..9] , length (show (x^exp)) == exp  ]
    

powers = takeWhile notEmpty $ map findPower [1..]
    where notEmpty xs = (length xs) > 0

solution = sum $ map length powers