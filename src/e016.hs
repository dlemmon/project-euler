{-
euler 16
-------


2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?




-}


import Data.Char

import Math

sumDigits x = sum $ digits x

solution = sumDigits $ 2^1000
