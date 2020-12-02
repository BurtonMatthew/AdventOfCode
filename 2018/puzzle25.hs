import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
    fileData <- readFile "input25.txt"
    let points = fst $ last $ readP_to_S parseFile fileData
    putStrLn $ (++) "Part 1: " $ show  $ solvePart1 points
    --putStrLn $ (++) "Part 2: " $ show $ solvePart2 (Registers 0 0 0 0 0 0) program

data Point = Point Int Int Int Int deriving(Eq, Ord, Show)

isDigitOrNegative '-' = True
isDigitOrNegative c = isDigit c

manDist :: Point -> Point -> Int
manDist (Point x y z w) (Point x' y' z' w') = abs (x'-x) + abs (y'-y) + abs (z'-z) + abs (w'-w)

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
    where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                        else x : go (Set.insert x s) xs

parsePoint :: ReadP Point
parsePoint = do
    x <- liftM read $ munch1 isDigitOrNegative
    char ','
    y <- liftM read $ munch1 isDigitOrNegative
    char ','
    z <- liftM read $ munch1 isDigitOrNegative
    char ','
    w <- liftM read $ munch1 isDigitOrNegative
    return (Point x y z w)

parseFile :: ReadP [Point]
parseFile = do
    points <- many1 $ do
        point <- parsePoint
        skipSpaces
        return point
    eof
    return points

solvePart1 :: [Point] -> Int
solvePart1 points = solvePart1' points [] [[]] where
    solvePart1' :: [Point] -> [Point] -> [[Point]] -> Int
    solvePart1' [] _ constellations = length $ filter (\c -> length c > 0) constellations
    solvePart1' (p:ps) [] constellations = solvePart1' ps [p] ([p]:constellations)
    solvePart1' ps (ex:exs) constellations = solvePart1' (foldl' (\ps' p -> delete p ps') ps adjPoints) (adjPoints ++ exs) ((adjPoints ++ head constellations) : (tail constellations)) where
        adjPoints :: [Point]
        adjPoints = filter (\p' -> manDist p' ex <=3 ) ps