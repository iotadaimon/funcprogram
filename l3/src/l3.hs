-- l3.hs - Load as a module with ghci.

simpson :: (Float -> Float) -> Float -> Float -> Int -> Float
simpson f a b n = h / 3 * sum (calcvalues f h a n)
    where h = (b - a) / fromIntegral n

calcvalues :: (Float -> Float) -> Float -> Float -> Int -> [Float]
calcvalues f h a n = f a : temp ++ [f (a + fromIntegral n * h)]
  where
    ks   = [1 .. n - 1]
    temp = calcvaluesr [] f a h ks

calcvaluesr :: [Float] -> (Float -> Float) -> Float -> Float -> [Int] -> [Float]
calcvaluesr ys f a h [] = ys
calcvaluesr ys f a h ks = calcvaluesr xs f a h (tail ks)
  where
    xs = ys ++ [(if k `mod` 2 == 1 then 4 else 2) * f (a + fromIntegral k * h)]
    k  = head ks
