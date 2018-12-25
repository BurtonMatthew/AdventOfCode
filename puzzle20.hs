import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace


main :: IO ()
main = do
    fileData <- readFile "input20.txt"
    putStrLn $ (++) "Part 1: " $ show $ maximum $ Map.elems $ solvePart1 fileData
    putStrLn $ (++) "Part 1: " $ show $ length $ filter (>=1000) $ Map.elems $ solvePart1 fileData

type DistMap = Map (Int,Int) Int
type StackFrame = (Int,Int,Int)
stackX (x,_,_) = x
stackY (_,y,_) = y
stackDepth (_,_,d) = d

-- This approach abuses the way the input was generated, all choice branches either loop back onto their starting point or deadend without intersection
-- A general case solution requires creating a new search head every time you hit a choice

solvePart1 :: String -> DistMap
solvePart1 tokens =  solvePart1' tokens [] Map.empty (0,0) 0 where
    solvePart1' :: String -> [StackFrame] -> DistMap -> (Int,Int) -> Int -> DistMap
    solvePart1' [] _ m _ _= m
    solvePart1' (t:ts) stack visited (x,y) depth
        | t == '(' = solvePart1' ts ((x,y,depth):stack) visited (x,y) depth
        | t == ')' = solvePart1' ts (tail stack) visited (stackX $ head $ stack, stackY $ head $ stack) (stackDepth $ head $ stack)
        | t == '|' = solvePart1' ts stack visited (stackX $ head $ stack, stackY $ head $ stack) (stackDepth $ head $ stack)
        | t == 'W' = solvePart1' ts stack (Map.insertWith min (x-1,y) (depth+1) visited) (x-1,y) (depth+1)
        | t == 'E' = solvePart1' ts stack (Map.insertWith min (x+1,y) (depth+1) visited) (x+1,y) (depth+1)
        | t == 'N' = solvePart1' ts stack (Map.insertWith min (x,y-1) (depth+1) visited) (x,y-1) (depth+1)
        | t == 'S' = solvePart1' ts stack (Map.insertWith min (x,y+1) (depth+1) visited) (x,y+1) (depth+1)
        | otherwise = solvePart1' ts stack visited (x,y) depth

{- Graveyard of old approaches that didn't work out quite right. A lot of these are either correct and slow or slightly buggy. These solve more general cases

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import Text.ParserCombinators.ReadP

data Token = W | N | S | E | Choice [[Token]] deriving (Show, Eq)

getTokens (Choice ts) = ts
getTokens _ = undefined

main :: IO ()
main = do
    fileData <- readFile "input20.txt"
    --let regex = fst $ head $ readP_to_S parseRegex fileData -- "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" -- 31
    --let regex = fst $ head $ readP_to_S parseRegex "^(SSS|EEESSSWWW)ENNES$" -- 8
    let regex = fst $ head $ readP_to_S parseRegex "^(E|SSEENNW)S$" -- 4
    --let regex = fst $ head $ readP_to_S parseRegex "^(E|SEN)$" -- 2
    --let regex = fst $ head $ readP_to_S parseRegex "^SEN$" -- 3
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 regex

{-
solvePart1 :: [Token] -> Int
solvePart1 tokens = maximum $ Map.elems $ solvePart1' tokens (0,0) 0 $ Map.singleton (0,0) 0 where
    solvePart1' :: [Token] -> (Int,Int) -> Int -> Map (Int,Int) Int -> Map (Int,Int) Int
    solvePart1' [] _ _ m = m
    solvePart1' (W:ts) (x,y) d m = solvePart1' ts (x-1,y) (d+1) $ Map.insertWith smaller (x-1,y) (d+1) m
    solvePart1' (E:ts) (x,y) d m = solvePart1' ts (x+1,y) (d+1) $ Map.insertWith smaller (x+1,y) (d+1) m
    solvePart1' (N:ts) (x,y) d m = solvePart1' ts (x,y-1) (d+1) $ Map.insertWith smaller (x,y-1) (d+1) m
    solvePart1' (S:ts) (x,y) d m = solvePart1' ts (x,y+1) (d+1) $ Map.insertWith smaller (x,y+1) (d+1) m
    solvePart1' ((Choice tokens):ts) p d m = foldl' (Map.unionWith smaller) Map.empty $ map (\toks -> solvePart1' (toks++ts) p d m) tokens where
    smaller a b = if a < b then a else b
-}

{-
data Head = Head
    {
        pos :: (Int,Int),
        tokens :: [[Token]]
    } deriving(Show, Eq)

solvePart1 :: [Token] -> Int
solvePart1 regex = maximum $ Map.elems $ solvePart1' [(Head (0,0) [regex])] 0 $ Map.singleton (0,0) 0 where
    solvePart1' :: [Head] -> Int -> Map (Int,Int) Int -> Map (Int,Int) Int
    solvePart1' [] _ m = m
    solvePart1' heads depth m = solvePart1' nextHeads (depth+1) (Map.unionWith smaller m $ Map.fromListWith smaller $ zip (map pos nextHeads) (repeat (depth+1))) where
        nextHeads = concatMap updateHead heads
        updateHead :: Head -> [Head]
        updateHead (Head p []) = []
        updateHead (Head p ([]:ts)) = updateHead (Head p ts)
        updateHead (Head (x,y) ((W:ts):t's)) = [Head (x-1,y) (ts:t's)]
        updateHead (Head (x,y) ((E:ts):t's)) = [Head (x+1,y) (ts:t's)]
        updateHead (Head (x,y) ((N:ts):t's)) = [Head (x,y-1) (ts:t's)]
        updateHead (Head (x,y) ((S:ts):t's)) = [Head (x,y+1) (ts:t's)]
        updateHead (Head p (((Choice options):ts):t's)) = concatMap (\o -> updateHead $ Head p (o:ts:t's)) options
        smaller a b = if a < b then a else b
-}

data Head = Head
    {
        pos :: (Int,Int),
        depth :: Int
    } deriving(Show, Eq)
type DistMap = Map (Int,Int) Int

solvePart1 :: [Token] -> Int
solvePart1 regex = maximum $ Map.elems $ snd $ solvePart1' regex [(Head (0,0) 0)] $ Map.singleton (0,0) 0 where
solvePart1' :: [Token] -> [Head] -> Map (Int,Int) Int -> ([Head], DistMap)
solvePart1' [] heads m = (heads, m)
solvePart1' (t:ts) heads m = solvePart1' ts nextHeads nextMap where
    nextHeads :: [Head]
    nextHeads = filter (\h -> (Map.notMember (pos h) m) || (m Map.! (pos h)) > depth h) $ concat $ map fst updateAllHeads
    nextMap :: Map (Int,Int) Int
    nextMap = foldl' (Map.unionWith min) m $ map snd $ updateAllHeads
    updateAllHeads :: [([Head], DistMap)]
    updateAllHeads = map updateHead $ heads
    updateHead :: Head -> ([Head], DistMap)
    updateHead (Head (x,y) d)
        | t == W =  ([Head (x-1,y) (d+1)], Map.singleton (x-1,y) (d+1))
        | t == E =  ([Head (x+1,y) (d+1)], Map.singleton (x+1,y) (d+1))
        | t == S =  ([Head (x,y+1) (d+1)], Map.singleton (x,y+1) (d+1))
        | t == N =  ([Head (x,y-1) (d+1)], Map.singleton (x,y-1) (d+1))
        | otherwise = (concat $ map fst recursedHeads, foldl' (Map.unionWith min) m $ map snd $ recursedHeads)  where
            --I've done something horribly wrong I've having to repeat all my logic
            recursedHeads :: [([Head], DistMap)]
            recursedHeads = map (\rts -> solvePart1' rts [(Head (x,y) d)] m) $ getTokens t


parseWest :: ReadP Token
parseWest = do
    char 'W'
    return W

parseEast :: ReadP Token
parseEast = do
    char 'E'
    return E

parseNorth :: ReadP Token
parseNorth = do
    char 'N'
    return N

parseSouth :: ReadP Token
parseSouth = do
    char 'S'
    return S

parseChoice :: ReadP Token
parseChoice = do
    char '('
    tokens <- sepBy (many parseToken) (char '|')
    char ')'
    return (Choice tokens)

parseToken :: ReadP Token
parseToken = do
    tok <- choice [parseWest,parseEast,parseNorth,parseSouth,parseChoice]
    return tok

parseRegex :: ReadP [Token]
parseRegex = do
    char '^'
    regex <- many1 parseToken
    char '$'
    return regex

-}