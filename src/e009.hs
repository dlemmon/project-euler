{-
euler 9
-------



A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.



-}

-- sum1000 (a,b,c) = a + b + c == 1000

pyt [a,b,c] = a^2 + b^2 == c^2

-- b = \/(c^2 - a^2) 

findTriplet = filter pyt [[a,b,1000 - a - b] | a <- [1..1000] , b <- [a..1000], (1000 - a) > 2*b ]
    


solution = product $ head findTriplet 