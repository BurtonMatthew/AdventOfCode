import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST
import Text.ParserCombinators.ReadP
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data Claim = Claim
    { claimId :: Int
    , area :: Rectangle
    } deriving(Show, Eq)

data Rectangle = Rectangle Int Int Int Int deriving(Show, Eq)

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input3.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ map (area . fst . head . readP_to_S parseClaim) fileLines
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ map (fst . head . readP_to_S parseClaim) fileLines



parsedigit :: ReadP Char
parsedigit = satisfy (\char -> char >= '0' && char <= '9')

parseRect :: ReadP Rectangle
parseRect = do
    x1 <- many1 parsedigit
    char ','
    y1 <- many1 parsedigit
    char ':'
    skipSpaces
    width <- many1 parsedigit
    char 'x'
    height <- many1 parsedigit
    return $ Rectangle (read x1) (read y1) (read x1 + read width - 1) (read y1 + read height - 1)

parseClaim :: ReadP Claim
parseClaim = do
    char '#'
    id <- many1 parsedigit
    string " @ "
    rect <- parseRect
    eof
    return $ Claim (read id) rect

solvePart1 :: [Rectangle] -> Int
solvePart1 rects = countContentiousCells $ runSTUArray $ do
    cloth <- newArray (0, (1000 * 1000 - 1)) (0 :: Int)
    mapM (\(x,y) -> addArray cloth (index2dTo1d (x,y)) 1) $ concatMap rectToIndices rects
    return cloth
    where
        addArray arr i n = do 
            val <- readArray arr i
            writeArray arr i (val+n)
        countContentiousCells = length . filter (>1) . arrToList
        arrToList arr = map ((!) arr) [0..999999]
        index2dTo1d (x,y) = x * 1000 + y 
        rectToIndices (Rectangle x1 y1 x2 y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

solvePart2 :: [Claim] -> Int
solvePart2 claims = claimId $ head $ filter (not . anyOverlap claims) claims
    where
        anyOverlap :: [Claim] -> Claim -> Bool
        anyOverlap claims claim = foldl' (\acc c -> acc || (overlap claim c)) False claims
        overlap :: Claim -> Claim -> Bool
        overlap left right
            | left == right = False
            | otherwise = overlapRect (area left) (area right)
        overlapRect :: Rectangle -> Rectangle -> Bool
        overlapRect (Rectangle lx1 ly1 lx2 ly2) (Rectangle rx1 ry1 rx2 ry2) = lx2 >= rx1 && lx1 <= rx2 && ly2 >= ry1 && ly1 <= ry2 
