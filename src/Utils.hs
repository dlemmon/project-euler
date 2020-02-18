module Utils where

  removeItem _ []                 = []
  removeItem x (y:ys) | x == y    = removeItem x ys
                      | otherwise = y : removeItem x ys