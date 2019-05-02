{-
euler 20
-------


sumDigits $ fact 100



-}




import Math

sumDigits x = sum $ digits x

solution = sumDigits $ fact 100
