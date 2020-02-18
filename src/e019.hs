{-
euler 19
-------


You are given the following information, but you may prefer to do some research for yourself.

  a.1 Jan 1900 was a Monday.
  b.Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  c.A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


-}

data Month = Ene | Feb | Mar | Abr | May | Jun | Jul | Ago | Sep | Oct | Nov | Dic deriving (Show, Eq, Ord, Enum, Bounded) 

leapyear year = if not $ divBy 4 then False
        else if not $ divBy 100  then True
        else if not $ divBy 400  then False
        else True
          where divBy n = mod year n == 0



yearDays year = if leapyear year then 366 else 365



monthDays (y,Feb) = if leapyear y then 29 else 28
monthDays (y,m)   = if (isEven && not greaterThanJul) || (not isEven && greaterThanJul) 
              then 31 else 30    
          where isEven = mod (fromEnum m) 2 == 0
                greaterThanJul = m > Jul
                

firstsunday (1900,Ene) = 6
firstsunday (year,Ene) = mod (firstsunday (prev,Ene) - yearDays prev) 7 
    where prev = year - 1

firstsunday (y,m) = mod (firstsunday (y,pred m) - monthDays (y,pred m)) 7   

solution = sum $ map isZero [firstsunday (y,toEnum m) | y <- [1901..2000], m <- [0..11]] 
    where isZero offset = if offset == 0 then 1 else 0




  
