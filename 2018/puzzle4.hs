import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data DateTime = DateTime 
    {
        year :: Int,
        month :: Int,
        day :: Int,
        hour :: Int,
        minute :: Int
    } deriving(Show, Eq, Ord)

data Event = Begin Int | Wake | Sleep deriving(Show,Eq,Ord)

data LogEntry = LogEntry DateTime Event deriving(Show,Eq,Ord)

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input4.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ map (fst . head . readP_to_S parseLogEntry) fileLines
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 $ map (fst . head . readP_to_S parseLogEntry) fileLines

parseLogEntry :: ReadP LogEntry
parseLogEntry = do
    dt <- parseDateTime
    skipSpaces
    e <- choice [parseBegin, parseWake, parseSleep]
    return (LogEntry dt e)

parseDateTime :: ReadP DateTime
parseDateTime = do
    char '['
    year <- liftM read $ munch1 isDigit
    char '-'
    month <- liftM read $ munch1 isDigit
    char '-'
    day <- liftM read $ munch1 isDigit
    char ' '
    hour <- liftM read $ munch1 isDigit
    char ':'
    minute <- liftM read $ munch1 isDigit
    char ']'
    return (DateTime year month day hour minute)

parseBegin :: ReadP Event
parseBegin = do
    string "Guard #"
    id <- liftM read $ munch1 isDigit
    string " begins shift"
    return (Begin id)

parseWake :: ReadP Event
parseWake = do
    string "wakes up"
    return Wake

parseSleep :: ReadP Event
parseSleep = do
    string "falls asleep"
    return Sleep

-- I leave this as a monument to the hubris of choosing the wrong data structre
solvePart1 :: [LogEntry] -> Int
solvePart1 logs = findSleepiest * findMinute
    where
        sleepMap = buildSleepMap 0 0 (sort logs) Map.empty
        buildSleepMap :: Int -> Int -> [LogEntry] -> Map (Int,Int) Int -> [((Int,Int), Int)]
        buildSleepMap _ _ [] sMap =  Map.toAscList sMap
        buildSleepMap guard sleepT (log:logs) sMap =
            case log of (LogEntry _ (Begin n)) -> buildSleepMap n sleepT logs sMap
                        (LogEntry (DateTime _ _ _ _ t) Sleep) -> buildSleepMap guard t logs sMap
                        (LogEntry (DateTime _ _ _ _ t) Wake) -> buildSleepMap guard sleepT logs $ foldl' (\m n -> Map.insertWith (+) (guard,n) 1 m) sMap [sleepT..t-1]
        findSleepiest = fst $ head $ sortBy greaterSnd $ map (foldl' sumGuardSlp (0,0)) $ (map . map) (\((g,t),n) -> (g,n)) $ groupBy sameGuard sleepMap
        sameGuard ((l,_),_) ((r,_),_) = l == r
        sumGuardSlp (_,t1) (g,t2) = (g,t1+t2)
        greaterSnd (_,l) (_,r) = compare r l
        findMinute = fst $ head $ sortBy greaterSnd $ map (\((_,t),n) -> (t,n)) $ filter (sameGuard ((findSleepiest,0),0)) sleepMap


-- Specifically didn't built a map here, but ended up using one anyway. Crazy world. Need to redo this whole day really. At least the code is pretty clean
solvePart2 :: [LogEntry] -> Int
solvePart2 logs = zipTuple2With (*) $ fst $ head $ sortBy compareCount $ countInstances sleepList
    where
        sleepList = buildSleepList 0 0 (sort logs) []
        buildSleepList :: Int -> Int -> [LogEntry] -> [(Int,Int)] -> [(Int,Int)]
        buildSleepList _ _ [] list = list
        buildSleepList guard sleepT (log:logs) list =
            case log of (LogEntry _ (Begin n)) -> buildSleepList n sleepT logs list
                        (LogEntry (DateTime _ _ _ _ t) Sleep) -> buildSleepList guard t logs list
                        (LogEntry (DateTime _ _ _ _ t) Wake) -> buildSleepList guard sleepT logs $ list ++ zip (repeat guard) [sleepT..t-1]
        compareCount ((_,_), l) ((_,_), r) = compare r l

countInstances :: (Ord a, Num b) => [a] -> [(a, b)]
countInstances keys = Map.toList $ Map.fromListWith (+) $ zip keys $ repeat 1

zipTuple2With :: (a->b->c) -> (a,b) -> c
zipTuple2With f (a,b) = f a b 