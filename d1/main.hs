import Data.List

-- common
parse :: String -> [[Int]]
parse = map sort . transpose . map (map read . words) . lines 

-- part 1
solveP1 :: Num a => [a] -> [a] -> a
solveP1 l1 l2 = sum $ map abs (zipWith (-) l1 l2)

-- part 2
counts :: Eq a => a -> [a] -> Int
counts a = length . filter (==a)

solveP2 :: [Int] -> [Int] -> Int
solveP2 l1 l2 = sum $ map (\x -> x * counts x l2) l1

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parse input
    let (l1,l2) = (head parsed, parsed !! 1)
    print ("Part 1 = " ++ show (solveP1 l1 l2))
    print ("Part 2 = " ++ show (solveP2 l1 l2))
    return ()
