{-
euler 2
-------

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.


-}
import Data.List

-- check if all string in the list are a permutation of characters
hasSameDigits :: Show a => [a] -> Bool
hasSameDigits []  = True
hasSameDigits [_] = True
hasSameDigits (a:b:xs) = compare (show a) (show b) && hasSameDigits xs
    -- compare two lists having some permutation of characters
    where compare x y = length x == length y && sort x == sort y

-- the list of all xs starting at 10^(n-1) such as the list of multiplications 
-- of this number to n have a permutation of the same digits
candidatesList :: (Integral a, Show a) => a -> [a]
candidatesList n = [x | x <- [10^(n-1)..] , hasSameDigits $ map ((*) x) [1..n] ]

solution :: Integer
solution = head $ candidatesList 6

{-
OUTPUTS
*Main> solution
142857
-}