-- l2.hs - Load as a module with ghci.

factorial :: Integer -> Integer
factorial n = product [1 .. n]

factc :: Integer -> Integer -> Integer
factc k n = factorial n `div` (factorial k * factorial (n - k))

recurc :: Integer -> Integer -> Integer
recurc k n | n > 0            = recurc (k - 1) (n - 1) + recurc k (n - 1)
           | k == n || k == 0 = 1
           | otherwise        = 0
