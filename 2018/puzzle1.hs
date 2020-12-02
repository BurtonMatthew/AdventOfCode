import Control.Monad

main :: IO ()
main = do
    fileLines <- liftM lines $ readFile "input1.txt"
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ map parseInput fileLines
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ map parseInput fileLines
    where
        parseInput :: String -> Int
        parseInput = read . filter (\x -> x /= '+')

solvePart1 :: [Int] -> Int
solvePart1 = sum

solvePart2 :: [Int] -> Int
solvePart2 ns = findRepeatedFreq [] 0 (cycle ns)
        where 
            findRepeatedFreq :: [Int] -> Int -> [Int] -> Int
            findRepeatedFreq freqs freq (n:ns)
                | freq `elem` freqs = freq
                | otherwise = findRepeatedFreq (freq:freqs) (freq+n) ns