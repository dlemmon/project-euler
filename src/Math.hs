
module Math where
  
  import Data.Char
  import Data.List
  import Data.Ratio
  import Primes
  
  fact n = partfact n 1

  -- !n / !m  | n > m
  partfact n m = product [n,(n-1)..(m+1)]
  
  digits x = map digitToInt $ show x
  
  combinations :: Int -> [a] -> [[a]]
  combinations n xs = if n>l then [] else bySize xs !! (l-n)
     where
        l = length xs
        bySize [] = [[[]]]
        bySize (x:xs) = let next = bySize xs
               in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
               
  divisors n = 1 : (nub (factors ++ (concat $ map combine [2..(length factors - 1)]))) 
    where combine len = map product $ combinations len factors
          factors = factorize n
          
          
  multiples n = map (*n) [1..]
          
  polygonalNum s n = div ((s - 2) * n * (n - 1)) 2 + n
  
  
  inverse x = (denominator x) % (numerator x)

  expandFract ([h])  = h % 1
  expandFract (h:hs) = h % 1 + (1 / ( expandFract hs)) 
  