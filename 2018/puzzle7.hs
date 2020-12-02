import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input7.txt")
    putStrLn $ (++) "Part 1: " $ solvePart1 $ parseRequirements fileLines
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ parseRequirements fileLines

parseRequirements :: [String] -> Map Char String
parseRequirements = foldl' parseRequirement Map.empty
    where
        parseRequirement :: Map Char String -> String -> Map Char String
        parseRequirement map str = Map.insertWith (++) (str !! 36) [(str !! 5)] map

solvePart1 :: Map Char String -> String
solvePart1 reqs = solvePart1' ['A'..'Z'] []
    where
        solvePart1' [] finished = reverse finished
        solvePart1' (pend:pends) finished 
            | canTakeAction finished pend = solvePart1' ['A'..'Z'] (pend:finished)
            | otherwise = solvePart1' pends finished
        canTakeAction finished pend = (not $ elem pend finished) && (all (\c -> elem c finished) $ fromMaybe [] $ Map.lookup pend reqs)

solvePart2 :: Map Char String -> Int
solvePart2 reqs = solvePart2' [] [] 5 0
    where
        solvePart2' :: String -> [(Char, Int)] -> Int -> Int -> Int
        solvePart2' fin inprog works time
            | works > 0 && nextWork /= '0' = solvePart2' fin ((nextWork, 61 + (ord nextWork - ord 'A')):inprog) (works-1) time
            | length inprog > 0 = solvePart2' ((fst smallestWork):fin) updateWorks (works+1) (snd smallestWork + time)
            | otherwise = time
            where
                nextWork = fromMaybe '0' $ safeHead $ filter canTakeAction ['A'..'Z']
                canTakeAction task = (not $ elem task fin) && (not $ elem task $ map fst inprog) && (all (\c -> elem c fin) $ fromMaybe [] $ Map.lookup task reqs)
                smallestWork = head $ sortBy (\(_,l) (_,r) -> compare l r) inprog
                updateWorks = map (\(c,t) -> (c,t - snd smallestWork)) $ filter (/= smallestWork) inprog

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a