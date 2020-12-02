import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input2.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 fileLines
    putStrLn $ (++) "Part 2: " $ solvePart2 fileLines

solvePart1 :: [String] -> Int
solvePart1 ids = (numWordsWithRepetition 2) * (numWordsWithRepetition 3)
    where
        letterCounts = map (Map.toList . countLetters) ids
        hasElemWithOccurances n = any (\(_,x) -> x == n)
        numWordsWithRepetition n = length $ filter (hasElemWithOccurances n) letterCounts

countLetters :: String -> Map Char Int
countLetters = countLetters' Map.empty
    where
        countLetters' :: Map Char Int -> String -> Map Char Int
        countLetters' dict [] = dict
        countLetters' dict (c:cs) = countLetters' (Map.insertWith (+) c 1 dict) cs

solvePart2 :: [String] -> String
solvePart2 ids = (\(x:y:ys) -> x `orderedIntersection` y) $ head $ filter (\(x:y:ys) -> countDiffs x y == 1) $ combinations 2 ids

countDiffs :: (Eq a) => [a] -> [a] -> Int
countDiffs xs ys = length $ filter (\(x,y) -> x /= y) $ zip xs ys

orderedIntersection :: (Eq a) => [a] -> [a] -> [a]
orderedIntersection xs ys = map fst $ filter (\(x,y) -> x == y) $ zip xs ys

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n ls = [(x:ys) | (x:xs) <- tails ls, ys <- combinations (n-1) xs]