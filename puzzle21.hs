import Control.Monad
import Data.Bits
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
    fileData <- readFile "input21.txt"
    let program = fst $ last $ readP_to_S parseFile fileData
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 (Registers 0 0 0 0 0 0) program
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 (Registers 0 0 0 0 0 0) program

data Registers = Registers
    {
        r0 :: Int,
        r1 :: Int,
        r2 :: Int,
        r3 :: Int,
        r4 :: Int,
        r5 :: Int
    } deriving (Show, Eq)

type OpCode = Int -> Int -> Int -> Registers -> Registers

data Program = Program Int (Map Int (Registers -> Registers))

updateRegister :: Int -> Int -> Registers -> Registers
updateRegister 0 n (Registers a b c d e f) = Registers n b c d e f
updateRegister 1 n (Registers a b c d e f) = Registers a n c d e f
updateRegister 2 n (Registers a b c d e f) = Registers a b n d e f
updateRegister 3 n (Registers a b c d e f) = Registers a b c n e f
updateRegister 4 n (Registers a b c d e f) = Registers a b c d n f
updateRegister 5 n (Registers a b c d e f) = Registers a b c d e n

readRegister :: Int -> Registers -> Int
readRegister 0 = r0
readRegister 1 = r1
readRegister 2 = r2
readRegister 3 = r3
readRegister 4 = r4
readRegister 5 = r5

addr :: OpCode
addr a b c r = updateRegister c (readRegister a r + readRegister b r) r
addi :: OpCode
addi a b c r = updateRegister c (readRegister a r + b) r
mulr :: OpCode
mulr a b c r = updateRegister c (readRegister a r * readRegister b r) r
muli :: OpCode
muli a b c r = updateRegister c (readRegister a r * b) r
banr :: OpCode
banr a b c r = updateRegister c (readRegister a r .&. readRegister b r) r
bani :: OpCode
bani a b c r = updateRegister c (readRegister a r .&. b) r
borr :: OpCode
borr a b c r = updateRegister c (readRegister a r .|. readRegister b r) r
bori :: OpCode
bori a b c r = updateRegister c (readRegister a r .|. b) r
setr :: OpCode
setr a _ c r = updateRegister c (readRegister a r) r
seti :: OpCode
seti a _ c r = updateRegister c a r
gtir :: OpCode
gtir a b c r = updateRegister c (if a > readRegister b r then 1 else 0) r
gtri :: OpCode
gtri a b c r = updateRegister c (if readRegister a r > b then 1 else 0) r
gtrr :: OpCode
gtrr a b c r = updateRegister c (if readRegister a r > readRegister b r then 1 else 0) r
eqir :: OpCode
eqir a b c r = updateRegister c (if a == readRegister b r then 1 else 0) r
eqri :: OpCode
eqri a b c r = updateRegister c (if readRegister a r == b then 1 else 0) r
eqrr :: OpCode
eqrr a b c r = updateRegister c (if readRegister a r == readRegister b r then 1 else 0) r

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving(Show, Eq, Read)
allOps = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]
runOp Addr = addr
runOp Addi = addi
runOp Mulr = mulr
runOp Muli = muli
runOp Banr = banr
runOp Bani = bani
runOp Borr = borr
runOp Bori = bori
runOp Setr = setr
runOp Seti = seti
runOp Gtir = gtir
runOp Gtri = gtri
runOp Gtrr = gtrr
runOp Eqir = eqir
runOp Eqri = eqri
runOp Eqrr = eqrr

capitalized :: String -> String
capitalized [] = []
capitalized (c:cs) = toUpper c : map toLower cs

parseFile :: ReadP Program
parseFile = do
    string "#ip"
    skipSpaces
    ip <- liftM read $ munch1 isDigit
    skipSpaces
    instructions <- many1 $ do
        f <- liftM read $ liftM capitalized $ munch1 isLetter
        skipSpaces
        a <- liftM read $ munch1 isDigit
        skipSpaces
        b <- liftM read $ munch1 isDigit
        skipSpaces
        c <- liftM read $ munch1 isDigit
        skipSpaces
        return ((runOp f) a b c)
    eof
    return (Program ip (Map.fromList $ zip [0..] instructions))


solvePart1 :: Registers -> Program -> Int
solvePart1 initRegisters (Program boundRegister instructions) = solvePart1' 0 initRegisters where
    maxInstruction = maximum $ Map.keys instructions 
    solvePart1' ip registers 
        | ip == 29 = r2 registers
        | otherwise = solvePart1' (readRegister boundRegister updatedRegister + 1) updatedRegister where
            updatedRegister = instructions Map.! ip $ updateRegister boundRegister ip registers

solvePart2 :: Registers -> Program -> Int
solvePart2 initRegisters (Program boundRegister instructions) = solvePart1' 0 initRegisters Set.empty (-1) where
    maxInstruction = maximum $ Map.keys instructions 
    solvePart1' ip registers prevSeen lastSeen 
        | ip == 29 && r2 registers `elem` prevSeen = lastSeen
        | ip == 29 = solvePart1' (readRegister boundRegister updatedRegister + 1) updatedRegister (Set.insert (r2 registers) prevSeen) (r2 registers)
        | otherwise = solvePart1' (readRegister boundRegister updatedRegister + 1) updatedRegister prevSeen lastSeen where
            updatedRegister = instructions Map.! ip $ updateRegister boundRegister ip registers