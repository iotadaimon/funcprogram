-- l4.hs - Run this with runghc.

main :: IO ()
main = do
    putStr "Please enter a set : "
    input <- readLn
    print (getAllCombinationsSet (input :: [Int]))

  
-- Get a set of all possible combinations of elements in xs
getAllCombinationsSet :: [a] -> [[a]]
getAllCombinationsSet [] = []
getAllCombinationsSet xs = getAllSubsets [xs] xs (length xs - 1)


-- Get a set of all possible combinations of xs. Each element in the resulting set is of the given length l.
getAllSubsets :: [[a]] -> [a] -> Int -> [[a]]
getAllSubsets xss _  0 = xss ++ [[]]
getAllSubsets xss xs l = xss ++ getAllSubsets (getAllSubsetsIter xs l indices endIndices) xs (l-1)
                        where indices = [0..l-1]
                              endIndices = let l' = length xs in [l'-l..l'-1]


-- Get a set of length l from xs. Each consecutive call should yield a new/unique combination of elements from xs.
getAllSubsetsIter :: [a] -> Int -> [Int] -> [Int] -> [[a]]
getAllSubsetsIter xs l indices endIndices 
    | indices == endIndices = [getElemAtIndices xs indices]
    | otherwise = getElemAtIndices xs indices : getAllSubsetsIter xs l (incrementIndices indices (length xs - 1) 0) endIndices


-- Get a list of elements from xs specified by their indices - ys.
getElemAtIndices :: [a] -> [Int] -> [a]
getElemAtIndices xs [y] = [xs !! y]
getElemAtIndices xs ys  = (xs !! head ys) : getElemAtIndices xs (tail ys)


-- If maxi = 5 then [1,2,5] -> [1,3,4]
incrementIndices :: [Int] -> Int -> Int -> [Int]
incrementIndices xs maxi depth
    | (depth /= (length xs - 1)) && (xs !! (depth + 1) == -1) = take (length xs - (length xs - depth)) xs ++ ascendingSequence (xs !! depth + 1) (length (drop depth xs)) 
    | xs !! depth < maxi - (length xs - depth) + 1 = 
        if length (drop depth xs) == 1
            then changeNthElement xs depth (xs !! depth + 1)
            else incrementIndices xs maxi (depth + 1)
    | otherwise = incrementIndices (changeNthElement xs depth (-1)) maxi (depth-1)


-- Create an ascending sequence of length l, starting from startN.
-- Example: ascendingSequence 5 3 -> [5,6,7]
ascendingSequence :: Int -> Int -> [Int]
ascendingSequence startN 1 = [startN]
ascendingSequence startN l = startN : ascendingSequence (startN + 1) (l - 1)


-- Take list xs and replace its n-th element with y. Then, return the modified list xs.
changeNthElement :: [a] -> Int -> a -> [a]
changeNthElement xs n y = take n xs ++ [y] ++ drop (n + 1) xs
