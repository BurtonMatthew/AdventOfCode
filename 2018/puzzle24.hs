import Control.Monad
import Data.Char
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
    fileData <- readFile "input24.txt"
    let squads = Map.fromList $ zip [0..] $ fst $ last $ readP_to_S parseFile fileData
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 squads
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 squads
    --mapM_ putStrLn $ map show $ Map.assocs squads

data Team = NoTeam | Immune | Infection deriving (Eq,Show)
data Squad = Squad
    {
        size :: Int,
        hp :: Int,
        damage :: Int,
        initiative :: Int,
        dmgType :: String,
        weaknesses :: [String],
        immunities :: [String],
        team :: Team
    } deriving (Show, Eq)

parseSquad :: ReadP Squad
parseSquad = do
    size <- liftM read $ munch1 isDigit
    skipSpaces
    string "unit"
    optional $ char 's'
    string " each with "
    hp <- liftM read $ munch1 isDigit
    string " hit points "
    (immunities, weaknesses) <- option ([],[]) parseResistances
    skipSpaces
    string "with an attack that does "
    damage <- liftM read $ munch1 isDigit
    skipSpaces
    dmgType <- munch1 isLetter
    string " damage at initiative "
    initiative <- liftM read $ munch1 isDigit
    return (Squad size hp damage initiative dmgType weaknesses immunities NoTeam)

parseResistances :: ReadP ([String], [String])
parseResistances = do
    char '('
    resists <- liftM (foldl' (\(as,bs) (a's,b's) -> (as ++ a's, bs ++ b's)) ([],[])) $ sepBy1 (choice [parseImmunity, parseWeakness]) (string "; ")
    char ')'
    return resists

parseImmunity = do
    string "immune to "
    immunities <- sepBy1 (munch1 isLetter) (string ", ")
    return (immunities,[])

parseWeakness = do
    string "weak to "
    weaknesses <- sepBy1 (munch1 isLetter) (string ", ")
    return ([],weaknesses)

parseFile :: ReadP [Squad]
parseFile = do
    string "Immune System:"
    skipSpaces
    immuneSquad <- many1 $ do
        squad <- parseSquad
        skipSpaces
        return (squad { team = Immune } )
    string "Infection:"
    skipSpaces
    teamInfection <- many1 $ do
        squad <- parseSquad
        skipSpaces
        return (squad { team = Infection } )
    eof
    return (immuneSquad ++ teamInfection)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a

solvePart1 :: Map Int Squad -> Int
solvePart1 squads 
    | done = sum $ map size $ Map.elems squads
    | otherwise = solvePart1 $ foldl' fightTarget squads $ buildTargetList where
        done :: Bool
        done = not (Immune `elem` (map team $ Map.elems squads)) || not (Infection `elem` (map team $ Map.elems squads))
        buildTargetList :: [(Int, Int, Int)]
        buildTargetList = reverse $ sort $ buildTargetList' (sortBy highPowHighInit $ Map.assocs squads) (Map.assocs squads)
        buildTargetList' :: [(Int, Squad)] -> [(Int,Squad)] -> [(Int, Int, Int)]
        buildTargetList' [] _ = []
        buildTargetList' (atk:atks) targs
            | bestTarget == Nothing = buildTargetList' atks targs
            | otherwise = (initiative $ snd $ atk, fst atk, fst $ fromJust bestTarget) : (buildTargetList' atks (delete (fromJust bestTarget) targs)) where
                bestTarget :: Maybe (Int,Squad)
                bestTarget = safeHead $ sortBy dmgPowInit $ filter viableTarget targs
                viableTarget (_,d) = (team $ snd $ atk) /= team d && dmgBetween (snd atk) d > 0
                dmgPowInit (_,d1) (_,d2) = (dmgBetween (snd atk) d2, size d2 * damage d2, initiative d2) `compare` (dmgBetween (snd atk) d1, size d1 * damage d1, initiative d1)
        highPowHighInit (_, sq) (_, sq1) = (damage sq1 * size sq1, initiative sq1) `compare` (damage sq * size sq, initiative sq)
        fightTarget :: Map Int Squad -> (Int, Int, Int) -> Map Int Squad
        fightTarget sq (_,atk,def) = Map.alter (doAttack $ Map.lookup atk sq) def sq
        doAttack :: Maybe Squad -> Maybe Squad -> Maybe Squad
        doAttack _ Nothing = Nothing
        doAttack Nothing _ = Nothing
        doAttack (Just atk) (Just def)
            | numKilled >= size def = Nothing
            | otherwise = Just $ def { size = (size def - numKilled) } where
                numKilled = dmgBetween atk def `quot` hp def
        dmgBetween atk def
            | dmgType atk `elem` immunities def = 0
            | dmgType atk `elem` weaknesses def = 2 * damage atk * size atk
            | otherwise = damage atk * size atk



solvePart2 :: Map Int Squad -> Int
solvePart2 initSquad = snd $ head $ filter (\(b,_) -> b) $ map(\i -> solvePart2' Map.empty $ Map.map (boost i) initSquad) [0..] where
    boost i sq = if team sq == Immune then sq { damage = (damage sq + i) } else sq
    solvePart2' :: Map Int Squad -> Map Int Squad -> (Bool,Int)
    solvePart2' prevSquads squads
        | done = (Immune `elem` (map team $ Map.elems squads), sum $ map size $ Map.elems squads)
        | squads == prevSquads = (False, 0)
        | otherwise = solvePart2' squads $ foldl' fightTarget squads $ buildTargetList where
            done :: Bool
            done = not (Immune `elem` (map team $ Map.elems squads)) || not (Infection `elem` (map team $ Map.elems squads))
            buildTargetList :: [(Int, Int, Int)]
            buildTargetList = reverse $ sort $ buildTargetList' (sortBy highPowHighInit $ Map.assocs squads) (Map.assocs squads)
            buildTargetList' :: [(Int, Squad)] -> [(Int,Squad)] -> [(Int, Int, Int)]
            buildTargetList' [] _ = []
            buildTargetList' (atk:atks) targs
                | bestTarget == Nothing = buildTargetList' atks targs
                | otherwise = (initiative $ snd $ atk, fst atk, fst $ fromJust bestTarget) : (buildTargetList' atks (delete (fromJust bestTarget) targs)) where
                    bestTarget :: Maybe (Int,Squad)
                    bestTarget = safeHead $ sortBy dmgPowInit $ filter viableTarget targs
                    viableTarget (_,d) = (team $ snd $ atk) /= team d && dmgBetween (snd atk) d > 0
                    dmgPowInit (_,d1) (_,d2) = (dmgBetween (snd atk) d2, size d2 * damage d2, initiative d2) `compare` (dmgBetween (snd atk) d1, size d1 * damage d1, initiative d1)
            highPowHighInit (_, sq) (_, sq1) = (damage sq1 * size sq1, initiative sq1) `compare` (damage sq * size sq, initiative sq)
            fightTarget :: Map Int Squad -> (Int, Int, Int) -> Map Int Squad
            fightTarget sq (_,atk,def) = Map.alter (doAttack $ Map.lookup atk sq) def sq
            doAttack :: Maybe Squad -> Maybe Squad -> Maybe Squad
            doAttack _ Nothing = Nothing
            doAttack Nothing def = def
            doAttack (Just atk) (Just def)
                | numKilled >= size def = Nothing
                | otherwise = Just $ def { size = (size def - numKilled) } where
                    numKilled = dmgBetween atk def `quot` hp def
            dmgBetween atk def
                | dmgType atk `elem` immunities def = 0
                | dmgType atk `elem` weaknesses def = 2 * damage atk * size atk
                | otherwise = damage atk * size atk