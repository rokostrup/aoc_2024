import Data.List

testStr = "123   456\n567 111\n7 11\n"

-- common
strToInt :: [String] -> [Int]
strToInt = map read

parse = map sort . transpose . map (strToInt . words) . lines 

-- part 1
solveP1 l1 l2 = sum $ map abs (zipWith (-) l1 l2)

-- part 2
counts a = length . filter (==a)

solveP2 l1 l2 = sum $ map (\x -> x * counts x l2) l1

main = do
    input <- readFile "input.txt"
    let parsed = parse input
    let (l1,l2) = (head parsed, parsed !! 1)
    print ("Part 1 = " ++ show (solveP1 l1 l2))
    print ("Part 2 = " ++ show (solveP2 l1 l2))
    return ()
