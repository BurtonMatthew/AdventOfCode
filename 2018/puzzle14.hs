import Control.Monad
import Data.Char
import Data.List
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 768071 [3,7]
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 768071 [3,7]

solvePart1 :: Int -> [Int] -> [Int]
solvePart1 its startVals = solvePart1' 2 initScores elves where
    initScores = Map.fromList $ zip [0..] startVals
    elves = take (length startVals) [0..]
    solvePart1' len scores elves 
        | len > its + 10 = map ((Map.!) scores) [its..its+9]
        | otherwise = solvePart1' newLen newScores newElves where
            newDigits = digits $ sum $ map ((Map.!) scores) elves
            newLen = length newDigits + len
            newScores = Map.union scores $ Map.fromList $ zip [len..] newDigits
            newElves = map (\n -> ((scores Map.! n)+1+n) `mod` newLen) elves
        
digits :: Integral x => x -> [x]
digits 0 = [0]
digits x = (reverse . digits') x where
    digits' 0 = []
    digits' x = x `mod` 10 : digits' (x `div` 10)


-- Very slow, should keep track of list matching as we go, but hey.
solvePart2 :: Int -> [Int] -> Int
solvePart2 goal startVals = solvePart2' 2 initScores elves startVals where
    initScores = Map.fromList $ zip [0..] startVals
    elves = take (length startVals) [0..]
    goalDigits = digits goal
    goalLen = length goalDigits
    solvePart2' :: Int -> Map Int Int -> [Int] -> [Int] -> Int
    solvePart2' len scores elves runDigits
        | findSublistIndex goalDigits runDigits /= Nothing = fromJust $ findSublistIndex goalDigits (Map.elems scores)
        | otherwise = solvePart2' newLen newScores newElves newRunDigits where
            newDigits = digits $ sum $ map ((Map.!) scores) elves
            newLen = length newDigits + len
            newScores = Map.union scores $ Map.fromList $ zip [len..] newDigits
            newElves = map (\n -> ((scores Map.! n)+1+n) `mod` newLen) elves
            newRunDigits = reverse $ take (goalLen+4) $ reverse $ runDigits ++ newDigits

findSublistIndex pattern list = findIndex (isPrefixOf pattern) (tails list) 