import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Debug.Trace

type MineMap = Array (Int,Int) Char
data Cart = Cart 
    {
        x :: Int,
        y :: Int,
        dir :: Char,
        turn :: Char
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input13.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 (parseMap fileLines) []
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 (parseMap fileLines) []
    --mapM_ putStrLn $ (++) ["Part 1:"] $ printMap $ parseMap $ fileLines
    --putStrLn $ (++) "Part 2: " $ show $ solvePart2 fileData

parseMap :: [String] -> (MineMap, [Cart])
parseMap lines = (buildArrayMap, buildCartList) where
    raw = ((concatMap . map) fixupCoord) . (map (zip [0..])) . transpose . (map (zip [0..])) $ lines -- old dumb way of doing things
    width = (length . head) lines
    height = length lines
    fixupCoord (y,(x,c)) = ((x,y),c)
    cartToTrack '<' = '-'
    cartToTrack '>' = '-'
    cartToTrack '^' = '|'
    cartToTrack 'v' = '|'
    cartToTrack c = c
    isCart ((x,y),c) = c `elem` "><^v"
    buildArrayMap = listArray ((0,0),(width-1,height-1)) $ (concatMap . map) cartToTrack $ transpose lines
    buildCartList = map (\((x,y),c) -> Cart x y c '<') $ filter isCart raw

printMap :: MineMap -> [String]
printMap arr = map(\y -> map (\x -> arr ! (x,y)) [0..fst $ snd $ bounds arr]) [0..snd $ snd $ bounds arr]
  
tickCart :: MineMap -> Cart -> Cart
tickCart arr (Cart x y dir turn)
    | dir == '>' && cur == '/'  = Cart x (y-1) '^' turn
    | dir == '>' && cur == '\\' = Cart x (y+1) 'v' turn
    | dir == '<' && cur == '/'  = Cart x (y+1) 'v' turn
    | dir == '<' && cur == '\\' = Cart x (y-1) '^' turn
    | dir == '^' && cur == '/'  = Cart (x+1) y '>' turn
    | dir == '^' && cur == '\\' = Cart (x-1) y '<' turn
    | dir == 'v' && cur == '/'  = Cart (x-1) y '<' turn
    | dir == 'v' && cur == '\\' = Cart (x+1) y '>' turn
    | dir == '>' && cur == '+' && turn == '<' = Cart x (y-1) '^' '-'
    | dir == '>' && cur == '+' && turn == '-' = Cart (x+1) y '>' '>'
    | dir == '>' && cur == '+' && turn == '>' = Cart x (y+1) 'v' '<'
    | dir == '<' && cur == '+' && turn == '<' = Cart x (y+1) 'v' '-'
    | dir == '<' && cur == '+' && turn == '-' = Cart (x-1) y '<' '>'
    | dir == '<' && cur == '+' && turn == '>' = Cart x (y-1) '^' '<'
    | dir == '^' && cur == '+' && turn == '<' = Cart (x-1) y '<' '-'
    | dir == '^' && cur == '+' && turn == '-' = Cart x (y-1) '^' '>'
    | dir == '^' && cur == '+' && turn == '>' = Cart (x+1) y '>' '<'
    | dir == 'v' && cur == '+' && turn == '<' = Cart (x+1) y '>' '-'
    | dir == 'v' && cur == '+' && turn == '-' = Cart x (y+1) 'v' '>'
    | dir == 'v' && cur == '+' && turn == '>' = Cart (x-1) y '<' '<'
    | dir == '>' = Cart (x+1) y dir turn
    | dir == '<' = Cart (x-1) y dir turn
    | dir == '^' = Cart x (y-1) dir turn
    | dir == 'v' = Cart x (y+1) dir turn
    | otherwise = undefined
    where
        cur = arr ! (x,y)

collides :: Cart -> Cart -> Bool
collides (Cart x1 y1 _ _) (Cart x2 y2 _ _) = x1 == x2 && y1 == y2

topToBottomLeftToRight :: Cart -> Cart -> Ordering
topToBottomLeftToRight (Cart x1 y1 _ _) (Cart x2 y2 _ _) = (y1, x1) `compare` (y2, x2)

solvePart1 :: (MineMap, [Cart]) -> [Cart] -> (Int,Int)
solvePart1 (arr, []) proc = solvePart1 (arr, sortBy topToBottomLeftToRight proc) []
solvePart1 (arr, (cart:carts)) proc
    | any (collides cart) carts || any (collides cart) proc = (x cart, y cart)
    | otherwise = solvePart1 (arr, carts) ((tickCart arr cart):proc)

-- BUGGED: DOES NOT HANDLE TEST INPUT SET CORRECTLY, OFF BY 1
solvePart2 :: (MineMap, [Cart]) -> [Cart] -> (Int,Int)
solvePart2 (arr, []) proc 
    | length proc > 1 = solvePart2 (arr, sortBy topToBottomLeftToRight proc) []
    | otherwise = (x $ head proc, y $ head proc)
solvePart2 (arr, (cart:carts)) proc
    | any (collides cart) carts || any (collides cart) proc = solvePart2 (arr, filter (not . (collides cart)) carts) (filter (not . (collides cart)) proc)
    | otherwise = solvePart2 (arr, carts) ((tickCart arr cart):proc)
        