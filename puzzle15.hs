import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace
import Text.ParserCombinators.ReadP

data Tile = Empty | Wall | Elf Int Int | Gob Int Int deriving (Show, Eq)
type Coord = (Int,Int)
type TileMap = Map Coord Tile
hp :: Tile -> Int
hp (Gob n _) = n
hp (Elf n _) = n
hp _ = 0

guid :: Tile -> Int
guid (Gob _ n) = n
guid (Elf _ n) = n
guid _ = 0

enemy :: Tile -> Tile -> Bool
enemy (Gob _ _) (Elf _ _) = True
enemy (Elf _ _) (Gob _ _) = True
enemy _ _ = False

isGob :: Tile -> Bool
isGob (Gob _ _) = True
isGob _ = False

isElf :: Tile -> Bool
isElf (Elf _ _) = True
isElf _ = False

isAlive :: Tile -> Bool
isAlive x = isGob x || isElf x

main :: IO ()
main = do
    fileLines <- liftM lines (readFile "input15.txt")
    let numElves = length $ filter ((==) 'E') $ concat fileLines
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 $ map (fst . head . readP_to_S parseMap) fileLines
    putStrLn $ (++) "Part 2: " $ show $ snd $ head $ filter (\x -> fst x == numElves) $ map (\i -> solvePart2 i $ map (fst . head . readP_to_S parseMap) fileLines) [3..]

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
    return (Gob 200 0)

parseElf :: ReadP Tile
parseElf = do
    char 'E'
    return (Elf 200 0)

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
    initMap = Map.mapWithKey assignIds $ twoDListToMap tileMap
    assignIds :: Coord -> Tile -> Tile
    assignIds (r,c) (Gob hp _) = Gob hp ((r*1000)+c)
    assignIds (r,c) (Elf hp _) = Elf hp ((r*1000)+c)
    assignIds _ t = t
    solvePart1' :: TileMap -> [(Coord, Int)] -> Int -> Int
    solvePart1' m [] turnNum  = solvePart1' m (buildTurnOrder m ) (turnNum+1)
    solvePart1' m (t:ts) turnNum
        | isGobVic m || isElfVic m = (sum $ map hp $ Map.elems m) * turnNum
        | otherwise = solvePart1' (takeTurn m t) ts turnNum 
    buildTurnOrder :: TileMap -> [(Coord, Int)]
    buildTurnOrder m = sortBy readOrd $ map (\(p,t) -> (p,guid t)) $ filter (\(_,x) -> x /= Wall && x /= Empty) $  Map.toList m
    readOrd :: (Coord,Int) -> (Coord,Int) -> Ordering
    readOrd ((r1,c1),_) ((r2,c2),_) = (r1, c1) `compare` (r2, c2)
    isGobVic :: TileMap -> Bool
    isGobVic m = (length $ filter isElf $ Map.elems m) == 0
    isElfVic :: TileMap -> Bool
    isElfVic m = (length $ filter isGob $ Map.elems m) == 0
    adjTiles :: Coord -> [Coord]
    adjTiles (row,col) = [(row-1,col),(row,col-1),(row,col+1),(row+1,col)]
    takeTurn :: TileMap -> (Coord,Int) -> TileMap
    takeTurn m (pos,id)
        | id /= (guid $ m Map.! pos) = m
        | not $ isAlive $ m Map.! pos =  m
        | length (adjEnems m pos) > 0 = takeDamage m $ pickTarget m pos
        | otherwise = move m pos
    adjEnems :: TileMap -> Coord -> [(Coord, Tile)]
    adjEnems m pos = filter (\t -> enemy (m Map.! pos) $ snd t) $ map (\(x,y) -> ((x,y), m Map.! (x,y))) (adjTiles pos)
    pickTarget :: TileMap -> Coord -> Maybe Coord
    pickTarget m pos = liftM fst $ safeHead $ sortBy orderTargets $ adjEnems m pos where
        orderTargets :: (Coord, Tile) -> (Coord, Tile) -> Ordering
        orderTargets ((r1,c1),t1) ((r2,c2),t2) = (hp t1, r1, c1) `compare` (hp t2, r2, c2) 
    takeDamage :: TileMap -> Maybe Coord -> TileMap
    takeDamage m Nothing = m
    takeDamage m (Just pos) = Map.update applyDamage pos m where
        applyDamage :: Tile -> Maybe Tile
        applyDamage (Elf hp id)
            | hp <= 3 = Just Empty
            | otherwise = Just $ Elf (hp-3) id
        applyDamage (Gob hp id)
            | hp <= 3 = Just Empty
            | otherwise = Just $ Gob (hp-3) id
        applyDamage _ = undefined
    move :: TileMap -> Coord -> TileMap
    move m pos = stepTowards $ findPaths $ listTargetPos where
        actorT :: Tile
        actorT = m Map.! pos
        listTargetPos :: [Coord]
        listTargetPos = filter (\p -> (m Map.! p) == Empty ) $ concatMap (adjTiles . fst) $ filter (\(_,t) -> enemy actorT t) $ Map.toList m
        findPaths :: [Coord] -> Maybe Coord
        findPaths goals = searchHull [[pos]] [pos]  where 
            searchHull :: [[Coord]] -> [Coord] -> Maybe Coord
            searchHull paths visited
                | any reachesGoal paths = safeHead $ tail $ reverse $ head $ sortBy goalTieBreaker $ filter reachesGoal paths
                | length newPaths == 0 = Nothing
                | otherwise = searchHull newPaths ((map head newPaths) ++ visited)
                where
                    newPaths = nubBy (\l r -> head l == head r) $ filter (\p -> length p > 0) $ concatMap (\p -> nextPath p visited) paths
            reachesGoal :: [Coord] -> Bool
            reachesGoal path = (head path) `elem` goals
            goalTieBreaker :: [Coord] -> [Coord] -> Ordering
            goalTieBreaker (l:ls) (r:rs) = l `compare` r
            adjEmpty :: Coord -> [Coord] -> [Coord]
            adjEmpty p visited = filter (\x -> ((m Map.! x) == Empty) && (not $ x `elem` visited)) $ adjTiles p
            nextPath :: [Coord] -> [Coord] -> [[Coord]]
            nextPath path visited = map(\p -> p:path) $ adjEmpty (head path) visited
        stepTowards :: Maybe Coord -> TileMap
        stepTowards (Just p) =  takeDamage newMap $ pickTarget newMap p where
            newMap = Map.insert pos Empty $ Map.insert p actorT m
        stepTowards Nothing = m

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a


solvePart2 :: Int -> [[Tile]] -> (Int,Int)
solvePart2 elfDmg tileMap = solvePart2' initMap [] (-1) where
    initMap = Map.mapWithKey assignIds $ twoDListToMap tileMap
    assignIds :: Coord -> Tile -> Tile
    assignIds (r,c) (Gob hp _) = Gob hp ((r*1000)+c)
    assignIds (r,c) (Elf hp _) = Elf hp ((r*1000)+c)
    assignIds _ t = t
    solvePart2' :: TileMap -> [(Coord, Int)] -> Int -> (Int,Int)
    solvePart2' m [] turnNum  = solvePart2' m (buildTurnOrder m ) (turnNum+1)
    solvePart2' m (t:ts) turnNum
        | isGobVic m || isElfVic m = ((length $ filter isElf $ Map.elems m), ((sum $ map hp $ Map.elems m) * turnNum))
        | otherwise = solvePart2' (takeTurn m t) ts turnNum 
    buildTurnOrder :: TileMap -> [(Coord, Int)]
    buildTurnOrder m = sortBy readOrd $ map (\(p,t) -> (p,guid t)) $ filter (\(_,x) -> x /= Wall && x /= Empty) $  Map.toList m
    readOrd :: (Coord,Int) -> (Coord,Int) -> Ordering
    readOrd ((r1,c1),_) ((r2,c2),_) = (r1, c1) `compare` (r2, c2)
    isGobVic :: TileMap -> Bool
    isGobVic m = (length $ filter isElf $ Map.elems m) == 0
    isElfVic :: TileMap -> Bool
    isElfVic m = (length $ filter isGob $ Map.elems m) == 0
    adjTiles :: Coord -> [Coord]
    adjTiles (row,col) = [(row-1,col),(row,col-1),(row,col+1),(row+1,col)]
    takeTurn :: TileMap -> (Coord,Int) -> TileMap
    takeTurn m (pos,id)
        | id /= (guid $ m Map.! pos) = m
        | not $ isAlive $ m Map.! pos =  m
        | length (adjEnems m pos) > 0 = takeDamage m $ pickTarget m pos
        | otherwise = move m pos
    adjEnems :: TileMap -> Coord -> [(Coord, Tile)]
    adjEnems m pos = filter (\t -> enemy (m Map.! pos) $ snd t) $ map (\(x,y) -> ((x,y), m Map.! (x,y))) (adjTiles pos)
    pickTarget :: TileMap -> Coord -> Maybe Coord
    pickTarget m pos = liftM fst $ safeHead $ sortBy orderTargets $ adjEnems m pos where
        orderTargets :: (Coord, Tile) -> (Coord, Tile) -> Ordering
        orderTargets ((r1,c1),t1) ((r2,c2),t2) = (hp t1, r1, c1) `compare` (hp t2, r2, c2) 
    takeDamage :: TileMap -> Maybe Coord -> TileMap
    takeDamage m Nothing = m
    takeDamage m (Just pos) = Map.update applyDamage pos m where
        applyDamage :: Tile -> Maybe Tile
        applyDamage (Elf hp id)
            | hp <= 3 = Just Empty
            | otherwise = Just $ Elf (hp-3) id
        applyDamage (Gob hp id)
            | hp <= elfDmg = Just Empty
            | otherwise = Just $ Gob (hp-elfDmg) id
        applyDamage _ = undefined
    move :: TileMap -> Coord -> TileMap
    move m pos = stepTowards $ findPaths $ listTargetPos where
        actorT :: Tile
        actorT = m Map.! pos
        listTargetPos :: [Coord]
        listTargetPos = filter (\p -> (m Map.! p) == Empty ) $ concatMap (adjTiles . fst) $ filter (\(_,t) -> enemy actorT t) $ Map.toList m
        findPaths :: [Coord] -> Maybe Coord
        findPaths goals = searchHull [[pos]] [pos]  where 
            searchHull :: [[Coord]] -> [Coord] -> Maybe Coord
            searchHull paths visited
                | any reachesGoal paths = safeHead $ tail $ reverse $ head $ sortBy goalTieBreaker $ filter reachesGoal paths
                | length newPaths == 0 = Nothing
                | otherwise = searchHull newPaths ((map head newPaths) ++ visited)
                where
                    newPaths = nubBy (\l r -> head l == head r) $ filter (\p -> length p > 0) $ concatMap (\p -> nextPath p visited) paths
            reachesGoal :: [Coord] -> Bool
            reachesGoal path = (head path) `elem` goals
            goalTieBreaker :: [Coord] -> [Coord] -> Ordering
            goalTieBreaker (l:ls) (r:rs) = l `compare` r
            adjEmpty :: Coord -> [Coord] -> [Coord]
            adjEmpty p visited = filter (\x -> ((m Map.! x) == Empty) && (not $ x `elem` visited)) $ adjTiles p
            nextPath :: [Coord] -> [Coord] -> [[Coord]]
            nextPath path visited = map(\p -> p:path) $ adjEmpty (head path) visited
        stepTowards :: Maybe Coord -> TileMap
        stepTowards (Just p) =  takeDamage newMap $ pickTarget newMap p where
            newMap = Map.insert pos Empty $ Map.insert p actorT m
        stepTowards Nothing = m