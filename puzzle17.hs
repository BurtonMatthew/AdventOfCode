import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import System.IO
import Text.ParserCombinators.ReadP


main :: IO ()
main = do
    fileData <- readFile "input17.txt"
    let clayLocations = Set.fromList $ ordNub $ fst $ head $ readP_to_S parseFile fileData
    --outh <- openFile "test.txt" WriteMode
    --hPutStrLn outh $ printMap clayLocations
    --writeFile "test.txt" $ unlines $ printMap $ clayLocations
    --mapM_ putStrLn $ printMap clayLocations
    --let gameMap = Array.listArray ((0,0),(49,49)) $ fst $ last $ readP_to_S parseMap fileData
    --putStrLn $ (++) "Part 1: " $ show $ solvePart1 clayLocations
    --putStrLn $ (++) "Part 2: " $ show $ solvePart2 1000000000 gameMap []
    putStrLn $ show $ length $ filter (\c -> c == '#') $ unlines $ printMap $ clayLocations

printMap :: Set (Int,Int) -> [String]
printMap ps = map displayRow [minY..maxY]
    where
        minX = head $ sort $ map fst $ Set.elems ps
        maxX = last $ sort $ map fst $ Set.elems ps
        minY = head $ sort $ map snd $ Set.elems ps
        maxY = last $ sort $ map snd $ Set.elems ps
        displayRow y = map (displayChar y) [minX-2..maxX+2]
        displayChar y x
            | x == 500 && y == minY = '+'
            | (x,y) `elem` ps = '#'
            | otherwise = '.'

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
    where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                        else x : go (Set.insert x s) xs

parseXRange :: ReadP [(Int,Int)]
parseXRange = do
    string "y="
    yVal <- liftM read $ munch1 isDigit
    char ','
    skipSpaces
    string "x="
    xMin <- liftM read $ munch1 isDigit
    string ".."
    xMax <- liftM read $ munch1 isDigit
    return [(x,yVal) | x <- [xMin..xMax]]

parseYRange :: ReadP [(Int,Int)]
parseYRange = do
    string "x="
    xVal <- liftM read $ munch1 isDigit
    char ','
    skipSpaces
    string "y="
    yMin <- liftM read $ munch1 isDigit
    string ".."
    yMax <- liftM read $ munch1 isDigit
    return [(xVal,y) | y <- [yMin..yMax]]

parseFile :: ReadP [(Int,Int)]
parseFile = do
    vals <- many1 $ do
        coords <- choice [parseXRange, parseYRange]
        skipSpaces
        return coords
    eof
    return (concat vals)

solvePart1 :: Set (Int,Int) -> Int
solvePart1 startingClay = (length $ Set.elems $ doWaterfall (500,0) startingClay Set.empty) -1 where
    maxY :: Int
    maxY = maximum $ map snd $ Set.elems startingClay
    doWaterfall :: (Int,Int) -> Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
    doWaterfall (x,y) clay water
        | y > maxY = water
        | not $ (x,y+1) `Set.member` clay = doWaterfall (x,y+1) clay (Set.insert (x,y) water)
        | bucketSpots /= Nothing = doWaterfall (x,y-1) (Set.union clay $ fromJust $ bucketSpots) (Set.union (fromJust bucketSpots) water)
        | otherwise = Set.insert (x,y) $ Set.union (spread (+(-1)) (x-1,y)) (spread (+1) (x+1,y))
        where
            bucketSpots :: Maybe (Set (Int,Int))
            bucketSpots = liftM2 Set.union (run (+(-1)) (x,y)) (run (+1) (x,y)) where
                run ::  (Int -> Int) -> (Int,Int) -> Maybe (Set (Int,Int))
                run f (x,y)
                    | (x,y) `Set.member` clay = Just Set.empty
                    | not $ (x,y+1) `Set.member` clay = Nothing
                    | otherwise = liftM2 Set.insert (Just (x,y)) $ run f (f x, y)
            spread :: (Int -> Int) -> (Int,Int) -> Set (Int,Int)
            spread f (x,y)
                | (x,y) `Set.member` clay = Set.empty
                | (x,y+1) `Set.member` clay = Set.insert (x,y) $ spread f (f x, y)
                | otherwise =  trace "recurse" $ doWaterfall (x,y+1) clay (Set.insert (x,y) water)