import Control.Monad
import Data.Char
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
    fileData <- readFile "input9.txt"
    putStrLn $ (++) "Part 1: " $ show $ (\(p,m) -> solvePart1 p m) $ fst $ head $ readP_to_S parseGame fileData
    putStrLn $ (++) "Part 2: " $ show $ (\(p,m) -> solvePart1 p (m*100)) $ fst $ head $ readP_to_S parseGame fileData

parseGame :: ReadP (Int, Int)
parseGame = do
    players <- liftM read $ munch1 isDigit
    string " players; last marble is worth "
    marbles <- liftM read $ munch1 isDigit
    return (players, marbles)

solvePart1 :: Int -> Int -> Int
solvePart1 players marbles = solvePart1' (Seq.singleton 0) (marbles) 0 (cycle [0..players-1]) (Seq.fromList $ take players $ repeat 0)
    where 
        marbs = marbles + 1
        solvePart1' :: Seq Int -> Int -> Int -> [Int] -> Seq Int -> Int
        solvePart1' placed 0 pos _ scores = Seq.index (Seq.reverse $ Seq.sort $ scores) 0
        solvePart1' placed numRem pos (pIdx:pIdxs) scores
            | (marbs - numRem) `mod` 23 == 0 = solvePart1' (Seq.deleteAt (shift (-7)) placed) (numRem-1) (shift (-7)) pIdxs updateScore
            | otherwise = solvePart1' (Seq.insertAt (shift 2) (marbs - numRem) placed) (numRem-1) (shift 2) pIdxs scores
            where
                shift n = (pos + n) `mod` (length placed)
                updateScore = Seq.adjust' ((+) (marbs - numRem + Seq.index placed (shift (-7)))) pIdx scores