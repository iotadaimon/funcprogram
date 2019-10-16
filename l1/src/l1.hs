-- l1.hs - Run this with runghc.
calc :: Floating a => a -> a -> a -> a -> a
calc a b c d = (-2.25 * (a + 2 * b * c)) / (b - d ** (1 / 2))

main :: IO()
main = print (calc 2 3.4 3 5 :: Double)