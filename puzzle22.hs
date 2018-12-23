import Algorithm.Search
import Control.Monad
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as Array
import Data.Bits
import Data.Char
import Data.Function.Memoize
import Data.Int
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
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 8112 (13,743)
    --putStrLn $ (++) "Part 2: " $ show $ solvePart2 8112 (13,743)
    putStrLn $ (++) "Part 2: " $ show $ solvePart2Memo 8112 (13,743)

solvePart1 :: Int -> (Int,Int) -> Int
solvePart1 depth (goalX, goalY) = (sum . map sum) $ (map . map) (\i-> geoIdxToErosionLevel i `mod` 3) $ genGeologicIndex where
    geoIdxToErosionLevel i = ((i + depth) `mod` 20183)
    genGeologicIndex :: [[Int]]
    genGeologicIndex = clearLast $ take (goalY+1) $ unfoldr nextGeoRow initGeoRow
    initGeoRow = take (goalX+1) $ map (*16807) [0..]
    nextGeoRow :: [Int] -> Maybe ([Int], [Int])
    nextGeoRow [] = Nothing
    nextGeoRow (v:vs) = Just ((v:vs), (v + 48271) : iterateRow (v+48271) vs) where
        iterateRow _ [] =  []    
        iterateRow lastVal (v:vs) = calcGeo : iterateRow calcGeo vs where
            calcGeo = geoIdxToErosionLevel lastVal * geoIdxToErosionLevel v
    clearLast a = init a ++ [(init (last a) ++ [0])]

data Equipment = Torch | Climbing | Neither deriving(Eq, Ord, Show)
data SearchNode = SearchNode
    {
        pos :: (Int,Int),
        equipped :: Equipment
    } deriving(Eq, Ord, Show)

isLegalEquipment :: Int8 -> Equipment -> Bool
isLegalEquipment 0 Neither = False
isLegalEquipment 1 Torch = False
isLegalEquipment 2 Climbing = False
isLegalEquipment _ _ = True

solvePart2 :: Int -> (Int,Int) -> Int
solvePart2 depth (goalX, goalY) = fst $ fromJust $ aStar neighbours dist (dist goal) (== goal) (SearchNode (0,0) Torch)  where
    -- AStar Funcs
    neighbours :: SearchNode -> [SearchNode]
    neighbours node 
        | pos node == (goalX,goalY) = [SearchNode (goalX,goalY) Torch]
        | otherwise = filter (legalMove node) [SearchNode p e | p <- adjInBounds (pos node), e <- [Torch,Climbing,Neither]]
    adjInBounds (x,y) = filter inBounds [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    inBounds (x,y) = x >= 0 && x <= goalX+spillOverX && y >= 0 && y <= goalY+spillOverY
    legalMove :: SearchNode -> SearchNode -> Bool
    legalMove n1 n2 = (isLegalEquipment (gameMap Array.! (pos n1)) (equipped n2)) && (isLegalEquipment (gameMap Array.! (pos n2)) (equipped n2))
    dist :: SearchNode -> SearchNode -> Int
    dist (SearchNode (x1,y1) e1) (SearchNode (x2,y2) e2) = (abs $ x2-x1) + (abs $ y2-y1) + if e1 /= e2 then 7 else 0
    goal = SearchNode (goalX,goalY) Torch
    -- Gen the map
    spillOverX = goalY * 10
    spillOverY = goalY * 10
    gameMap :: UArray (Int,Int) Int8
    gameMap = (Array.listArray ((0,0), (goalX + spillOverX, goalY + spillOverY)) $ concat $ transpose $ (map . map) (\i-> fromIntegral $ geoIdxToErosionLevel i `mod` 3) $ genGeologicIndex) Array.// [((goalX,goalY), 0)]
    geoIdxToErosionLevel i = ((i + depth) `mod` 20183)
    genGeologicIndex = take (goalY+spillOverY+1) $ unfoldr nextGeoRow initGeoRow
    initGeoRow = take (goalX+spillOverX+1) $ map (*16807) [0..]
    nextGeoRow [] = Nothing
    nextGeoRow (v:vs) = Just ((v:vs), (v + 48271) : iterateRow (v+48271) vs) where
        iterateRow _ [] =  []    
        iterateRow lastVal (v:vs) = calcGeo : iterateRow calcGeo vs where
            calcGeo = geoIdxToErosionLevel lastVal * geoIdxToErosionLevel v


solvePart2Memo :: Int -> (Int,Int) -> Int
solvePart2Memo depth (goalX, goalY) = fst $ fromJust $ aStar neighbours dist (dist goal) (== goal) (SearchNode (0,0) Torch)  where
    -- AStar Funcs
    neighbours :: SearchNode -> [SearchNode]
    neighbours node 
        | pos node == (goalX,goalY) = [SearchNode (goalX,goalY) Torch]
        | otherwise = filter (legalMove node) [SearchNode p e | p <- adjInBounds (pos node), e <- [Torch,Climbing,Neither]]
    adjInBounds (x,y) = filter inBounds [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    inBounds (x,y) = x >= 0 && y >= 0
    legalMove :: SearchNode -> SearchNode -> Bool
    legalMove n1 n2 = (isLegalEquipment (calcGrid (pos n1)) (equipped n2)) && (isLegalEquipment (calcGrid (pos n2)) (equipped n2))
    dist :: SearchNode -> SearchNode -> Int
    dist (SearchNode (x1,y1) e1) (SearchNode (x2,y2) e2) = (abs $ x2-x1) + (abs $ y2-y1) + if e1 /= e2 then 7 else 0
    goal = SearchNode (goalX,goalY) Torch
    -- Gen map, memoized
    calcGrid :: (Int,Int) -> Int8
    calcGrid (x,y) = fromIntegral $ calcErosion x y `mod` 3
    calcErosion = memoize2 calcErosion'
    calcErosion' 0 y = geoToEro $ 48271 * y
    calcErosion' x 0 = geoToEro $ 16807 * x
    calcErosion' x y 
        | x == goalX && y == goalY = geoToEro $ 0
        | otherwise = geoToEro $ (calcErosion (x-1) y) * (calcErosion x (y-1))
    geoToEro i = (i + depth) `mod` 20183