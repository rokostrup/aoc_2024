import Data.List

testStr = "123   456\n567 111\n7 11\n7 7\n7 7\n7 7\n"

strToInt :: [String] -> [Int]
strToInt = map read

counts a = length . filter (==a)

solver l = 
    let xs = map sort . transpose . map (strToInt . words) . lines $ l
        l1 = head xs
        l2 = xs !! 1
     in sum $ map (\x -> x * counts x l2) l1

main = do
    input <- readFile "input.txt"
    let sol = solver input
    print ("part 1 = " ++ show sol)
    return ()
