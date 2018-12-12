import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Map (fromListWith, toList)
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.ReadP

data Pot = Plant | Soil deriving (Show, Eq)
data Rule = Rule [Pot] Pot deriving (Show, Eq)
data State = State [Pot] [Rule] deriving (Show, Eq)

main :: IO ()
main = do
    fileData <- readFile "input12.txt"
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 20 $ fst $ head $ readP_to_S parseState fileData
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 50000000000 $ fst $ head $ readP_to_S parseState fileData

parseState :: ReadP State
parseState = do
    string "initial state:"
    skipSpaces
    init <- many1 parsePot
    skipSpaces
    rules <- many1 parseRule
    eof
    return (State init rules)

parseRule :: ReadP Rule
parseRule = do
    condition <- many1 parsePot
    skipSpaces
    string "=>"
    skipSpaces
    result <- parsePot
    skipSpaces
    return (Rule condition result)

parsePot :: ReadP Pot
parsePot = do
    c <- choice [parsePlant, parseSoil]
    return c

parsePlant :: ReadP Pot
parsePlant = do
    char '#'
    return Plant

parseSoil :: ReadP Pot
parseSoil = do
    char '.'
    return Soil

solvePart1 :: Int -> State -> Int
solvePart1 gens (State init rules) = solvePart1' gens rules initPotList
    where 
        initPotList = zip [(-(gens*3))..(-1)] (repeat Soil) ++ zip [0..] (init ++ (take (gens*3) $ repeat Soil))
        solvePart1' 0 _ pots = sum $ map fst $ filter (\(_,p) -> p == Plant) pots
        solvePart1' gens rules pots = solvePart1' (gens-1) rules $ concatMap (procChunks rules) $ chunks 5 pots
        procChunks rules pots = [(fst $ head $ drop 2 pots, fromMaybe Soil $ foldl' (runRule pots) (Just Soil) rules)]
        runRule pots p (Rule pattern Soil)
            | (map snd pots) == pattern = Nothing
            | otherwise = p
        runRule pots p (Rule pattern Plant)
            | p == Nothing = Nothing
            | (map snd pots) == pattern = Just Plant
            | otherwise = p

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr chunk 
    where
        chunk [] = Nothing
        chunk (a:as)
            | length as >= (n-1) = Just (a : take (n-1) as, as)
            | otherwise = Nothing

solvePart2 :: Int -> State -> Int
solvePart2 gens state = solvePart2' 1 $ (solvePart1 1 state) : take 4 (repeat 0)
    where
        solvePart2' gen scores
            | gen == gens = head scores
            | (length . nub . diffs) scores == 1 = ((gens - gen) * (head . diffs) scores) + head scores
            | otherwise = solvePart2' (gen+1) $ solvePart1 (gen+1) state : init scores

diffs [] = []
diffs ls = zipWith (-) ls (tail ls)