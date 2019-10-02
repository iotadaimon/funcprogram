-- Load as a module with ghci.

simpson :: (Float -> Float) -> Float -> Float -> Int -> Float
simpson f a b n = do
                    let h = (b - a) / (fromIntegral n)
                    h / 3 * sum (calcvalues f h a n)

calcvalues :: (Float -> Float) -> Float -> Float -> Int -> [Float]
calcvalues f h a n = do
                        let ks = [1..n-1]
                        let temp = calcvaluesr [] f a h ks
                        f a : temp ++ [(f (a + (fromIntegral n) * h))]

calcvaluesr :: [Float] -> (Float -> Float) -> Float -> Float -> [Int] -> [Float]
calcvaluesr ys f a h ks = do
                            if null ks
                            then
                                ys
                            else
                                do
                                    let k = head ks
                                    calcvaluesr (ys ++ [(if k `mod` 2 == 1 then 4 else 2) * (f (a + (fromIntegral k) * h))]) f a h (tail ks)

