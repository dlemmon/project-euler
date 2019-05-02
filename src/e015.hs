{-
euler 15
-------



Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

  #--+--+
  #  |  |
  #######
  |  |  #
  +--+--#
  
How many such routes are there through a 20×20 grid?



-}


import Data.List

import Math

-- paths n = nub $ permutations ((replicate n 0) ++ (replicate n 1))
      

-- Mississippi Rule: To count the number of distinct permutations of a string of letters 
-- where not all of the letters are necessarily distinct, we count the number of 
-- permutations as if the letters were distinct and then divide by the number of equivalent 
-- permutations that can be made by permuting identical letters.
-- 
-- The problem is equivalent to the permutations of a  mississippi string with 10 D (going Down) and 10 R (going Right)
-- these are the only two optimal directions 

-- 40! / (20! * 20!) 
mississ n = div (partfact (n*2) n) (fact n)

solution = mississ 20
