{-
euler 14
-------



The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.



-}
import Data.List
import Data.Map
import Data.Ord


term n  | even n    = div n 2
        | otherwise       = 3*n + 1
        
-- I'm going off the rails on a lazy chain
chain [] _ = []
chain l stop  = l ++ if next == stop then [next] else chain [next] stop
    where next = term $ last l

    


solution = fst $ maximumBy (comparing snd) $ toList mapping

mapping = fromList [(i, collatz i) | i <- [1..1000000]]

-- return the collatz chain length of a given number 
collatz 1 = 0
collatz x = if next <= 1000000 then 1 + mapping ! next
        else 1 + collatz next
          where next = term x



