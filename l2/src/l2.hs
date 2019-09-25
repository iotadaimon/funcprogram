-- l2.hs - Load as a module with ghci.

factorial n = product [1..n]

factc k n = factorial n `div` ( factorial k * factorial (n - k) )

recurc k n = if n > 0
                then recurc (k - 1) (n - 1) + recurc k (n - 1)
                else if k == n || k == 0
                    then 1
                    else 0
