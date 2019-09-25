-- l1.hs - Run this with runghc.

main = do
    let a = 2
    let b = 3.4
    let c = 3
    let d = 5
    let x = (-2.25 * (a + 2 * b * c)) / (b - d ** (1 / 2))
    putStrLn (show x)
