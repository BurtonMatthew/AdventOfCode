import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = do
    fileData <- liftM (head . lines) (readFile "input4.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 fileData
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 fileData

solvePart1 :: String -> Int
solvePart1 = length . (solvePart1' [])
    where
        solvePart1' processed [] = reverse processed
        solvePart1' [] (c:cs) = solvePart1' [c] cs
        solvePart1' processed (c:cs)
            | head processed `reacts` c = solvePart1' (tail processed) cs
            | otherwise = solvePart1' (c:processed) cs
        reacts l r = l /= r && toLower l == toLower r

solvePart2 :: String -> Int
solvePart2 str = head $ sort $ map (solvePart1 . filterPolymers) ['a'..'z']
    where
        filterPolymers n = filter (\l -> toLower l /= toLower n) str