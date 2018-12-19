import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace
import Text.ParserCombinators.ReadP

data Tile = Empty | Wall | Elf Int | Gob Int deriving (Show, Eq)
type Coord = (Int,Int)
type TileMap = Map Coord Tile
hp :: Tile -> Int
hp (Gob n) = n
hp (Elf n) = n
hp _ = 0

enemy :: Tile -> Tile -> Bool
enemy (Gob _) (Elf _) = True
enemy (Elf _) (Gob _) = True
enemy _ _ = False

isGob :: Tile -> Bool
isGob (Gob _) = True
isGob _ = False

isElf :: Tile -> Bool
isElf (Elf _) = True
isElf _ = False

isAlive :: Tile -> Bool
isAlive x = isGob x || isElf x

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input15-test.txt")
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ map (fst . head . readP_to_S parseMap) fileLines
    --putStrLn $ (++) "Part 2: " $ show $ solvePart1 $  map (fst . head . readP_to_S parseCoord) fileLines

parseEmpty :: ReadP Tile
parseEmpty = do
    char '.'
    return Empty

parseWall :: ReadP Tile
parseWall = do
    char '#'
    return Wall

parseGob :: ReadP Tile
parseGob = do
    char 'G'
    return (Gob 200)

parseElf :: ReadP Tile
parseElf = do
    char 'E'
    return (Elf 200)

parseMap :: ReadP [Tile]
parseMap = do
    let rows = lines
    tiles <- many1 $ choice [parseEmpty, parseWall, parseGob, parseElf]
    eof
    return tiles

twoDListToMap :: [[a]] -> Map Coord a
twoDListToMap ns = Map.fromList $ zip [(x,y) | x <- [0..width-1], y <- [0..height-1]] $ concat ns where
    width = length $ head ns
    height = length ns

solvePart1 :: [[Tile]] -> Int
solvePart1 tileMap = solvePart1' initMap [] (-1) where
    initMap = twoDListToMap tileMap
    solvePart1' :: TileMap -> [Coord] -> Int -> Int
    solvePart1' m [] turnNum  = solvePart1' m (buildTurnOrder m ) (turnNum+1)
    solvePart1' m (t:ts) turnNum
        | isGobVic m || isElfVic m = (sum $ map hp $ Map.elems m) * turnNum
        | otherwise = solvePart1' (takeTurn m t) ts turnNum 
    buildTurnOrder :: TileMap -> [Coord]
    buildTurnOrder m = sortBy readOrd $ map fst $ filter (\(_,x) -> x /= Wall && x /= Empty) $  Map.toList m
    readOrd :: Coord -> Coord -> Ordering
    readOrd (x1,y1) (x2,y2) = (y1, x1) `compare` (y2, x2)
    isGobVic :: TileMap -> Bool
    isGobVic m = (length $ filter isElf $ Map.elems m) == 0
    isElfVic :: TileMap -> Bool
    isElfVic m = (length $ filter isGob $ Map.elems m) == 0
    adjTiles :: Coord -> [Coord]
    adjTiles (x,y) = [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]
    takeTurn :: TileMap -> Coord -> TileMap
    takeTurn m pos
        | not $ isAlive $ m Map.! pos = m
        | length (adjEnems m pos) > 0 = takeDamage m $ pickTarget m pos
        | otherwise = move m pos
    adjEnems :: TileMap -> Coord -> [(Coord, Tile)]
    adjEnems m pos = filter (\t -> enemy (m Map.! pos) $ snd t) $ map (\(x,y) -> ((x,y), m Map.! (x,y))) (adjTiles pos)
    pickTarget :: TileMap -> Coord -> Coord
    pickTarget m pos = fst $ head $ sortBy orderTargets $ adjEnems m pos where
        orderTargets :: (Coord, Tile) -> (Coord, Tile) -> Ordering
        orderTargets ((x1,y1),t1) ((x2,y2),t2) = (hp t1, y1, x1) `compare` (hp t2, y2, x2) 
    takeDamage :: TileMap -> Coord -> TileMap
    takeDamage m pos = Map.update applyDamage pos m where
        applyDamage :: Tile -> Maybe Tile
        applyDamage (Elf hp)
            | hp <= 3 = Just Empty
            | otherwise = Just $ Elf (hp-3)
        applyDamage (Gob hp)
            | hp <= 3 = Just Empty
            | otherwise = Just $ Gob (hp-3)
        applyDamage _ = undefined
    move :: TileMap -> Coord -> TileMap
    move m pos = stepTowards $ findPaths $ listTargetPos where
        actorT :: Tile
        actorT = m Map.! pos
        listTargetPos :: [Coord]
        listTargetPos = filter (\p -> (m Map.! p) == Empty ) $ concatMap (adjTiles . fst) $ filter (\(_,t) -> enemy actorT t) $ Map.toList m
        findPaths :: [Coord] -> Coord
        findPaths goals = searchHull  where 
            --adjPathable = (\p -> filter (\p2 -> (m Map.! p2) == Empty) $ adjTiles p)
            -- (coord, path to this point)
            -- | any toSearch is a goal, return that path
            -- | otherwise, expand ring 
        stepTowards :: Coord -> TileMap
        stepTowards = undefined

