{-# LANGUAGE FlexibleContexts #-}

{-
euler 94
-------

Almost equilateral triangles


It is easily proved that no equilateral triangle exists with integral length sides and integral area. 
However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and 
the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and 
area and whose perimeters do not exceed one billion (1,000,000,000).



-}

import Data.Ratio
import Data.List

isInt a = (a == (fromIntegral $ truncate a))

isIntArea a b = isInt hh && mod (b*(truncate hh)) 2 == 0  
    where hh = h (fromIntegral a) (fromIntegral b)

h a b = sqrt $ (a^2 - (b/2)^2)

areaT a b =  (b * (h a b)) / 2 

perim a b = a * 2 + b
{-
x > a * 2 + (a+c) = a*3 + c

x > a*3 - 1 
x > a*3 + 1
(x + 1)/3 > a
-}


pairs n = let limit = div (n+1) 3 in 
    [(a, b) | a <- [1..limit] , c <- [-1,1], let b = a+c, b > 0, isIntArea a b  ]
      

solution = sum $ map (uncurry perim) (pairs n)
    where n = 1000000000
-- fractions maxd = sort [(x % y) |   y <- [1..maxd], x <- [1..(y-1)], gcd x y == 1  ]

{-
empty list = length list == 0


findLast maxd currd a b = if maxd == currd then nexta
           else findLast maxd (currd+1) nexta b
     where  fractList = takeWhile (< b) ([(x % currd) |  x <- [(numerator a)..(currd-1)] ])            
            nexta = if empty fractList then a else maximum ([(last fractList),a]) 
            

           

solution = findLast 1000000 999950 0 (3%7)
-}