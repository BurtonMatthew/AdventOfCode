import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
    fileLines <- liftM lines $ readFile "input10.txt"
    mapM_ putStrLn $ (++) ["Part 1:"] $ prettyPrint $ solvePart1 $ map (fst . head . readP_to_S parsePosVel) fileLines
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ map (fst . head . readP_to_S parsePosVel) fileLines

isDigitOrNegative a = isDigit a || a == '-'

parsePosVel :: ReadP (Int,Int,Int,Int)
parsePosVel = do
    string "position=<"
    skipSpaces
    x <- liftM read $ munch1 isDigitOrNegative
    char ','
    skipSpaces
    y <- liftM read $ munch1 isDigitOrNegative
    char '>'
    skipSpaces
    string "velocity=<"
    skipSpaces
    vx <- liftM read $ munch1 isDigitOrNegative
    char ','
    skipSpaces
    vy <- liftM read $ munch1 isDigitOrNegative
    char '>'
    return (x,y,vx,vy)

solvePart1 :: [(Int,Int,Int,Int)] -> [(Int,Int)]
solvePart1 ps = solvePart1' ps (boxArea ps)
    where
        boxArea ps = (maxX ps - minX ps) * (maxY ps - minY ps)
        minX ps = head $ sort $ map (\(x,_,_,_) -> x) ps
        maxX ps = last $ sort $ map (\(x,_,_,_) -> x) ps
        minY ps = head $ sort $ map (\(_,y,_,_) -> y) ps
        maxY ps = last $ sort $ map (\(_,y,_,_) -> y) ps
        solvePart1' ps area
            | boxArea (newPs ps) > area = map (\(x,y,_,_) -> (x,y)) ps
            | otherwise = solvePart1' (newPs ps) (boxArea $ newPs ps)
        newPs ps = map (\(x,y,vx,vy) -> (x+vx,y+vy,vx,vy)) ps

prettyPrint :: [(Int,Int)] -> [String]
prettyPrint ps = map displayRow [minY..maxY]
    where
        minX = head $ sort $ map fst ps
        maxX = last $ sort $ map fst ps
        minY = head $ sort $ map snd ps
        maxY = last $ sort $ map snd ps
        displayRow y = map (displayChar y) [minX..maxX]
        displayChar y x
            | (x,y) `elem` ps = '#'
            | otherwise = '.'


solvePart2 :: [(Int,Int,Int,Int)] -> Int
solvePart2 ps = solvePart2' ps (boxArea ps) 0
    where
        boxArea ps = (maxX ps - minX ps) * (maxY ps - minY ps)
        minX ps = head $ sort $ map (\(x,_,_,_) -> x) ps
        maxX ps = last $ sort $ map (\(x,_,_,_) -> x) ps
        minY ps = head $ sort $ map (\(_,y,_,_) -> y) ps
        maxY ps = last $ sort $ map (\(_,y,_,_) -> y) ps
        solvePart2' ps area t
            | boxArea (newPs ps) > area = t
            | otherwise = solvePart2' (newPs ps) (boxArea $ newPs ps) (t+1)
        newPs ps = map (\(x,y,vx,vy) -> (x+vx,y+vy,vx,vy)) ps