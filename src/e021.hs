{-
euler 20
-------

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.


-}

import Primes
import Math
import Data.List
import Data.Tuple

          
d n = sum $ divisors n

amicablePair :: Integer -> [Integer]
amicablePair n = if n /= d' && d d' == n then sort [n, d n] else []
      where d' = d n
      

amicablesTill n = foldl nextAmic [] [1..n]
      where nextAmic amicables x =  if elem x amicables  
              then amicables 
              else amicables ++ (amicablePair x) 
              
solution = sum $ amicablesTill (10000-1)
  

