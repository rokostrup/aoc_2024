import Data.List

-- common
parse :: String -> [[Int]]
parse = map (map read . words) . lines 

delta :: [Int] -> [Int]
delta xs = zipWith (-) (tail xs) xs

isUnsafe :: (Ord t, Num t) => (t -> Bool) -> t -> Bool
isUnsafe p x = ((>3) . abs) x || p x 

getUnsafeAsc :: [Int] -> [Int]
getUnsafeAsc = findIndices (isUnsafe (>=0))
getUnsafeDesc :: [Int] -> [Int]
getUnsafeDesc = findIndices (isUnsafe (<=0))

addNextIndexOrNothing :: Num a => [a] -> [a]
addNextIndexOrNothing xs = if null xs then xs else xs ++ [last xs + 1]

minLenList :: Foldable t => t a -> t a -> t a
minLenList l1 l2 = if length l1 > length l2 then l2 else l1

getUnsafeIndices :: [Int] -> [Int]
getUnsafeIndices xs = addNextIndexOrNothing (minLenList (getUnsafeAsc xs) (getUnsafeDesc xs))

-- part 1
solveP1 :: [[Int]] -> Int
solveP1 = length . filter (null . getUnsafeIndices) . map delta

-- part 2
-- using the following approach:
-- we check the index of all unsafe entries. it is safe which can happen when no unsafe 
-- entries are found or when one or more adjacent unsafe entries are found. 
-- if multiple non-adjacent are found then it cannot be made valid
-- also, the cause of failure can always be at either the index or the next, so 
-- we need to add the next adjacent index to try out
isAdjacent [] = True
isAdjacent [x] = True
isAdjacent (x1:x2:xs) = x1 + 1 == x2 && isAdjacent (x2:xs) 

removeByIndex :: Int -> [a] -> [a]
removeByIndex i xs =  
    let (p1,p2) = splitAt i xs
     in p1 ++ tail p2

tryWithoutIndex :: [Int] -> Int -> Bool
tryWithoutIndex xs i = 
    let try = delta $ removeByIndex i xs
     in null $ getUnsafeIndices try

canBeMadeSafe :: [Int] -> [Int] -> Bool
canBeMadeSafe usi xs = isAdjacent usi && any (tryWithoutIndex xs) usi

isSafe :: [Int] -> Bool
isSafe xs = 
    let usi = getUnsafeIndices (delta xs)
     in null usi || canBeMadeSafe usi xs

solveP2 :: [[Int]] -> Int
solveP2 = length . filter isSafe

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parse input
    print ("Part 1 = " ++ show (solveP1 parsed))
    print ("Part 2 = " ++ show (solveP2 parsed))
    return ()
