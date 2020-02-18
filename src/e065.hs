{-
euler 65
-------

The square root of 2 can be written as an infinite continued fraction.

2–√=1+12+12+12+12+...

The infinite continued fraction can be written, 2–√=[1;(2)]
, (2) indicates that 2 repeats ad infinitum. In a similar way, 23−−√=[4;(1,3,1,8)]

.

It turns out that the sequence of partial values of continued fractions 
for square roots provide the best rational approximations. 
Let us consider the convergents for 2–√

.

1+12=321+12+12=751+12+12+12=17121+12+12+12+12=4129

Hence the sequence of the first ten convergents for 2–√

are:

1,32,75,1712,4129,9970,239169,577408,1393985,33632378,...

What is most surprising is that the important mathematical constant,
e=[2;1,2,1,1,4,1,1,6,1,...,1,2k,1,...]

.

The first ten terms in the sequence of convergents for e are:

2,3,8/3,11/4,19/7,8732,10639,19371,1264465,1457/536,...

The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17

.

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e
.
-}

import Math
import Data.Ratio


eFract = 2 : (concat $ map (\x->[1,(x*2),1]) [1..])


sumDigitsEFract n = sum $ digits $ numerator $ expandFract $ take n eFract 

solution = sumDigitsEFract 100

-- 272