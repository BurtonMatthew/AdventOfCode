import Control.Monad
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
    fileData <- readFile "input8.txt"
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ fst $ head $ readP_to_S parseTree fileData
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ fst $ head $ readP_to_S parseTree fileData

data Tree = Tree [Tree] [Int] deriving(Show)

parseMeta :: ReadP Int
parseMeta = do
    meta <- liftM read $ munch1 isDigit
    skipSpaces
    return meta

parseTree :: ReadP Tree
parseTree = do
    numChildren <- liftM read $ munch1 isDigit
    skipSpaces
    numMetas <- liftM read $ munch1 isDigit
    skipSpaces
    trees <- forM [1..numChildren] (\_ -> parseTree)
    metas <- forM [1..numMetas] (\_ -> parseMeta)
    return (Tree trees metas)

solvePart1 :: Tree -> Int
solvePart1 (Tree children metas) = sum metas + (sum $ map solvePart1 children)

solvePart2 :: Tree -> Int
solvePart2 (Tree [] metas) = sum metas
solvePart2 (Tree children metas) = sum $ map solvePart2 $ catMaybes $ map (\n -> safeIndex children (n-1)) metas

safeIndex :: [a] -> Int -> Maybe a
safeIndex list n
    | n+1 > length list = Nothing
    | otherwise = Just (list !! n)