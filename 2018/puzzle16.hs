import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

data Registers = Registers
    {
        r0 :: Int,
        r1 :: Int,
        r2 :: Int,
        r3 :: Int
    } deriving (Show, Eq)

data Example = Example
    {
        before :: Registers,
        after :: Registers,
        op :: Int,
        a :: Int,
        b :: Int,
        c :: Int
    } deriving (Show)

type OpCode = Int -> Int -> Int -> Registers -> Registers

updateRegister :: Int -> Int -> Registers -> Registers
updateRegister 0 n (Registers a b c d) = Registers n b c d
updateRegister 1 n (Registers a b c d) = Registers a n c d
updateRegister 2 n (Registers a b c d) = Registers a b n d
updateRegister 3 n (Registers a b c d) = Registers a b c n

readRegister :: Int -> Registers -> Int
readRegister 0 = r0
readRegister 1 = r1
readRegister 2 = r2
readRegister 3 = r3

main :: IO ()
main = do
    fileData <- readFile "input16.txt"
    let (examples, program) = fst $ last $ readP_to_S parseFile fileData
    putStrLn $ (++) "Part 1: " $ show $ solvePart1 examples
    putStrLn $ (++) "Part 2: " $ show $ solvePart2 examples program


allOpCodes :: [OpCode]
allOpCodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

addr :: Int -> Int -> Int -> Registers -> Registers
addr a b c r = updateRegister c (readRegister a r + readRegister b r) r
addi :: Int -> Int -> Int -> Registers -> Registers
addi a b c r = updateRegister c (readRegister a r + b) r
mulr :: Int -> Int -> Int -> Registers -> Registers
mulr a b c r = updateRegister c (readRegister a r * readRegister b r) r
muli :: Int -> Int -> Int -> Registers -> Registers
muli a b c r = updateRegister c (readRegister a r * b) r
banr :: Int -> Int -> Int -> Registers -> Registers
banr a b c r = updateRegister c (readRegister a r .&. readRegister b r) r
bani :: Int -> Int -> Int -> Registers -> Registers
bani a b c r = updateRegister c (readRegister a r .&. b) r
borr :: Int -> Int -> Int -> Registers -> Registers
borr a b c r = updateRegister c (readRegister a r .|. readRegister b r) r
bori :: Int -> Int -> Int -> Registers -> Registers
bori a b c r = updateRegister c (readRegister a r .|. b) r
setr :: Int -> Int -> Int -> Registers -> Registers
setr a _ c r = updateRegister c (readRegister a r) r
seti :: Int -> Int -> Int -> Registers -> Registers
seti a _ c r = updateRegister c a r
gtir :: Int -> Int -> Int -> Registers -> Registers
gtir a b c r = updateRegister c (if a > readRegister b r then 1 else 0) r
gtri :: Int -> Int -> Int -> Registers -> Registers
gtri a b c r = updateRegister c (if readRegister a r > b then 1 else 0) r
gtrr :: Int -> Int -> Int -> Registers -> Registers
gtrr a b c r = updateRegister c (if readRegister a r > readRegister b r then 1 else 0) r
eqir :: Int -> Int -> Int -> Registers -> Registers
eqir a b c r = updateRegister c (if a == readRegister b r then 1 else 0) r
eqri :: Int -> Int -> Int -> Registers -> Registers
eqri a b c r = updateRegister c (if readRegister a r == b then 1 else 0) r
eqrr :: Int -> Int -> Int -> Registers -> Registers
eqrr a b c r = updateRegister c (if readRegister a r == readRegister b r then 1 else 0) r

getPossibleOpCodes :: Example -> [OpCode]
getPossibleOpCodes (Example r1 r2 _ a b c) = filter (\f -> r2 == f a b c r1) allOpCodes

parseRegisters :: ReadP Registers
parseRegisters = do
    char '['
    r0 <- liftM read $ munch1 isDigit
    char ','
    skipSpaces
    r1 <- liftM read $ munch1 isDigit
    char ','
    skipSpaces
    r2 <- liftM read $ munch1 isDigit
    char ','
    skipSpaces
    r3 <- liftM read $ munch1 isDigit
    char ']'
    return (Registers r0 r1 r2 r3)

parseExample :: ReadP Example
parseExample = do
    string "Before:"
    skipSpaces
    before <- parseRegisters
    skipSpaces
    op <- liftM read $ munch1 isDigit
    skipSpaces
    a <- liftM read $ munch1 isDigit
    skipSpaces
    b <- liftM read $ munch1 isDigit
    skipSpaces
    c <- liftM read $ munch1 isDigit
    skipSpaces
    string "After:"
    skipSpaces
    after <- parseRegisters
    return (Example before after op a b c)


parseFile :: ReadP ([Example], [(Int,Int,Int,Int)])
parseFile = do
    examples <- many1 $ do
        ex <- parseExample
        skipSpaces
        return ex
    operations <- many1 $ do
        op <- liftM read $ munch1 isDigit
        skipSpaces
        a <- liftM read $ munch1 isDigit
        skipSpaces
        b <- liftM read $ munch1 isDigit
        skipSpaces
        c <- liftM read $ munch1 isDigit
        skipSpaces
        return (op, a, b, c)
    eof
    return (examples, operations)

solvePart1 :: [Example] -> Int
solvePart1 exs = length $ filter (\ex -> length (getPossibleOpCodes ex) > 2) exs

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving(Show, Eq)
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

getPossibleOps :: Example -> [Op]
getPossibleOps (Example r1 r2 _ a b c) = filter (\f -> r2 == (runOp f) a b c r1) allOps

solvePart2 :: [Example] -> [(Int, Int, Int, Int)] -> Int
solvePart2 examples instructions = r0 $ foldl' (\r (op, a, b, c) -> (opMap Map.! op) a b c r) (Registers 0 0 0 0) instructions where
    opMap :: Map Int OpCode
    opMap = peelUnique Map.empty possibleOptSet where
        possibleOptSet = map (\i -> (i, foldl' intersect allOps $ map getPossibleOps $ filter (\ex -> op ex == i) examples)) [0..15]
        peelUnique :: Map Int OpCode -> [(Int, [Op])] -> Map Int OpCode
        peelUnique m [] = m
        peelUnique m set = peelUnique (Map.union m $ mapUnique) (map (\(i,l) -> (i, filter (\x -> x /= uniqueOp) l)) $ delete unique set) where
            unique :: (Int, [Op])
            unique = head $ filter (\(_,l) -> length l == 1) set
            mapUnique :: Map Int OpCode
            mapUnique = (\(i,l) -> Map.singleton i $ runOp $ head l) unique
            uniqueOp :: Op
            uniqueOp = head $ snd $ unique