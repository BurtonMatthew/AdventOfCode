import Control.Monad
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Char
import Data.List
import Data.Map (fromListWith, toList)
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.ReadP

data Space = Empty | Tree | Lumberyard deriving (Show, Eq)

main :: IO ()
main = do
    fileData <- readFile "input18.txt"
    let gameMap = Array.listArray ((0,0),(49,49)) $ fst $ last $ readP_to_S parseMap fileData
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 10 gameMap
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 1000000000 gameMap []

parseEmpty :: ReadP Space
parseEmpty = do
    char '.'
    return Empty

parseTree :: ReadP Space
parseTree = do
    char '|'
    return Tree

parseLumberyard :: ReadP Space
parseLumberyard = do
    char '#'
    return Lumberyard

parseMap :: ReadP [Space]
parseMap = do
    tiles <- many1 $ do
        tile <- choice [parseEmpty, parseTree, parseLumberyard]
        skipSpaces
        return tile
    eof
    return tiles

solvePart1 :: Int -> Array (Int,Int) Space -> Int
solvePart1 0 m = countSpace Tree * countSpace Lumberyard where
    countSpace t = length $ filter ((==) t) $ Array.elems m    
solvePart1 iterations m = solvePart1 (iterations-1) $ Array.listArray ((0,0),bounds) $ map tickSpace $ Array.assocs m where
    bounds = snd $ Array.bounds m
    tickSpace (i, Empty) = if countAdjacent i Tree > 2 then Tree else Empty
    tickSpace (i, Tree) = if countAdjacent i Lumberyard > 2 then Lumberyard else Tree
    tickSpace (i, Lumberyard) = if (countAdjacent i Tree > 0) && (countAdjacent i Lumberyard > 0) then Lumberyard else Empty
    countAdjacent p t = length $ filter (\x -> m Array.! x == t) $ adj p
    adj (x1,y1) = [(x,y) | x <- [x1-1..x1+1], y <- [y1-1..y1+1], x >= 0, y >= 0, x <= fst bounds, y <= snd bounds, x /= x1 || y /= y1 ]

solvePart2 :: Int -> Array (Int,Int) Space -> [Array (Int,Int) Space] -> Int
solvePart2 iterations m prevStates
    | cycleIdx /= Nothing = calcScore $ head $ drop (iterations `mod` cycleLen) $ reverse $ take cycleLen prevStates
    | otherwise = solvePart2 (iterations-1) (Array.listArray ((0,0),bounds) $ map tickSpace $ Array.assocs m) (m:prevStates) where
    bounds = snd $ Array.bounds m
    tickSpace (i, Empty) = if countAdjacent i Tree > 2 then Tree else Empty
    tickSpace (i, Tree) = if countAdjacent i Lumberyard > 2 then Lumberyard else Tree
    tickSpace (i, Lumberyard) = if (countAdjacent i Tree > 0) && (countAdjacent i Lumberyard > 0) then Lumberyard else Empty
    countAdjacent p t = length $ filter (\x -> m Array.! x == t) $ adj p
    adj (x1,y1) = [(x,y) | x <- [x1-1..x1+1], y <- [y1-1..y1+1], x >= 0, y >= 0, x <= fst bounds, y <= snd bounds, x /= x1 || y /= y1 ]
    calcScore state = countSpace state Tree * countSpace state Lumberyard
    countSpace state t = length $ filter ((==) t) $ Array.elems state
    cycleIdx = elemIndex m prevStates
    cycleLen = 1 + fromJust cycleIdx
    
    

diffs [] = []
diffs ls = zipWith (-) ls (tail ls)