
module Primes where
  import Data.Array.Unboxed

  -- isqrt :: Integral a => a -> a   
  isqrt = floor . sqrt . fromIntegral
  
  divisibleBy i x = mod i x == 0
  -- without 2,3
  -- prime6 n = [ x | x <- [[5,7] ,[11,13]..[n-2,n]]]
  prime6 n = if (n<7) then [5,7] else gen ++ (map ((+) 6) $ drop (length gen - 2) gen) 
      where gen = prime6 (n-6)
  
  -- primes :: Integral a => a -> [a]  
  primes 1 = []
  primes n = 2:[i | i <- [3,5..n], not $ any (divisibleBy i) (primes (isqrt i))]
   
  --isprime :: Integral a => a -> Bool  
  isprime' n = last (primesToNA n) == n
  
  isprime n = isprime6 n && minPrimeFact n == n
  
  -- 6n+-1
  isprime6 n = mod6 == 1 || mod6 == 5
      where mod6 = mod n 6
 
  minPrimeFact n = head list
      where list =  dropWhile (not . divisibleBy n) $ (primesToNA (isqrt n)) ++ [n]  
      
  factorize n 
    | fact == n   = [n]
    | otherwise   = fact : factorize (div n fact) 
        where fact = minPrimeFact n
        
  maxPrimeFact n = last $ factorize n
  
  -- primesToNA :: Integral a => a -> [a]  
  primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
    where
      ar = f 5 $ accumArray (\ a b -> False) True (3,n) 
                          [(i,()) | i <- [9,15..n]]
      f p a | q > n = a
            | True  = if null x then a2 else f (head x) a2
        where q = p^2
              a2  :: UArray Int Bool
              a2 = a // [(i,False) | i <- [q, q+2*p..n]]
              x  = [i | i <- [p+2,p+4..n], a2 ! i]
              
              
  -- mrsList :: Integral a => a -> [a]
  mrsList 1 = [0]
  mrsList n = (2^(n-1)-1) : mrsList (n-1)
  
  mrsPrimesList n = map mrs $ filter lucasLehmer $ primesToNA n
  
  mrs p = 2^p - 1

  
  s mp 1 = 4 `mod` mp
  s mp n = ((s mp $ n-1)^2-2) `mod` mp
   
  lucasLehmer 2 = True
  lucasLehmer p = s (2^p-1) (p-1) == 0
     


              
              