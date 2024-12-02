import Data.List

-- common
parse :: String -> [[Int]]
parse = map (map read . words) . lines 

-- part 1
isOrderedBy :: (a -> a -> Bool) -> [a] -> Bool
isOrderedBy _ [] = True
isOrderedBy _ [x] = True
isOrderedBy p (x0:x1:xs) = p x0 x1 && isOrderedBy p (x1:xs)

isOrdered :: Ord a => [a] -> Bool
isOrdered xs = isOrderedBy (<) xs || isOrderedBy (>) xs

maxDiff3 :: (Ord b, Num b) => [b] -> Bool
maxDiff3 l1 = not $ any ((> 3) . abs) (zipWith (-) l1 (tail l1))

solveP1 :: [[Int]] -> Int
solveP1 = length . filter (\x -> isOrdered x && maxDiff3 x) 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parse input
    print ("Part 1 = " ++ show (solveP1 parsed))
    return ()
