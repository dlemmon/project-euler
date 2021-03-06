{-
euler 57
-------


It is possible to show that the square root of two can be expressed as an infinite continued fraction.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
[1,2,2,2,2]

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, 
is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?



-}


-- (map f bs) `using` parList rdeepseq
import Math 
import Data.Ratio
import Control.Parallel.Strategies

moreDigitsNum x = digits (numerator x) > digits (denominator x)
    where digits x = length $ show x 

howMany :: Int -> [Int]
howMany until = using convs (parList rdeepseq)
    where makeList x = take x sqrt2Fract
          convs = map (countMore . expandFract . makeList) [1..until] 
          countMore x = if moreDigitsNum x then 1 else 0  

   
    
sqrt2Fract = 1:(repeat 2)
    
solution = sum $ howMany 1000


