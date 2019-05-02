{-
euler 41
-------

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?


-}

import Primes
import Data.Char
import Data.List

pandigital :: Int -> [Integer]
pandigital d = map toInt $ permutations ['1'..(intToDigit d)]
    where toInt x = read x::Integer

reversePandigitals d = reverse $ sort $ pandigital d


get (Just x) = x 

solution = get $ foldl findFirstPrime Nothing  [9,8..4]
    where findFirstPrime acc digits = if acc == Nothing 
            then find isprime $ reversePandigitals digits 
            else acc  
