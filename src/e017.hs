{-
euler 17
-------



If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" 
when writing out numbers is in compliance with British usage.


-}

import qualified Data.Map as M

uniqueNames = M.fromList ( [  (0,"zero"),(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5,"five"),
              (6,"six"),(7,"seven"),(8,"eight"),(9,"nine"),
              (10,"ten"),(11,"eleven"),(12,"twelve"),  (20,"twenty"), 
              (100,"hundred"),(1000,"thousand")]
      ++ suffixer 10 1 "teen" ++ suffixer 0 10 "ty") ;

specialPrefix = [(3,"thir"),(4,"for"),(5,"fif"),(6,"six"),(7,"seven"),(8,"eigh"),(9,"nine")];


suffixer start mult suffix = map multiplyAndConcat specialPrefix
    where multiplyAndConcat (n, pref) = (start + n*mult, pref ++ suffix )

name n = if n == 1000 then (uniqueNames M.! 1) ++ " " ++ (uniqueNames M.! n)
  else if n > 99 
  then (uniqueNames M.! (div n 100)) ++ " " ++ (uniqueNames M.! 100) 
        ++ if (mod n 100 == 0 ) then "" 
          else (if (div n 100 /= 1) then " and " else " ") ++ (name $ mod n 100) 
  else if M.lookup n uniqueNames /= Nothing then (uniqueNames M.! n) 
  else (uniqueNames M.! ((div n 10)*10)) ++ "-" ++ (uniqueNames M.! (mod n 10))
    
countletters phrase = sum $ map chartonum phrase  
    where chartonum c = if (c == '-') || (c == ' ') then 0 else 1
    
solution = sum $ map (countletters . name) [1..1000] 
