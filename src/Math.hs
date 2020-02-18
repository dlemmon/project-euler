
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
  

  -- getContFract number 1 = [floor number]
  getContFract number = if snd p == 0.0 then [fst p] 
          else if fst p > 10^10 then []
          else (fst p) : getContFract (1/(snd p))
      where p = properFraction number
      
  --  fract properties
  --  [..,a,1] = [..,a+1]
  --  0   [0,1]
  --  1/2 [0,2]              =         [0,1,1]
  --  1/3 [0,3]  = [0,2,1]   |     2/3 [0,1,2]   =  [0,1,1,1]     
  --  1/4 [0,4]  = [0,3,1]   |     2/5 [0,2,2]   =  [0,2,1,1]   |  3/4 [0,1,3] = [0,1,2,1]  | 3/5 [0,1,1,2] = [0,1,1,1,1]

  
  reciprocalFract f = if head f == 0 
          then tail f else 0 : f  
          
  convergents fract = map getConv $ init $ scanl (\x y -> x + 1) 1 fract 
        where getConv n = expandFract $ take n fract 