{-
euler 1
-------
If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

-}

filterMultiples d y = or $ map (\x -> mod y x == 0) d

sumMultiplesBelow d y =  foldl (+) 0 multiples
    where multiples = filter (filterMultiples d) [2..(y-1)]
    
solution = sumMultiplesBelow [3,5] 1000 
 