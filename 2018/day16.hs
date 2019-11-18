{-# Language DataKinds #-}


import           Data.Bits
import           Data.Finite
import           Data.Maybe
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Set          as S
import qualified Data.Vector.Sized as V


type Parser      = Parsec Void String
type Registers   = V.Vector 4 Int
type Sample      = (Registers, Registers, Instruction)
type Instruction = (Finite 16, Finite 4, Finite 4, Finite 4)

data OpCode = ADDR | ADDI |
              MULR | MULI |
              BANR | BANI |
              BORR | BORI |
              SETR | SETI |
              GTIR | GTRI | GTRR |
              EQIR | EQRI | EQRR
              deriving (Enum, Bounded, Ord, Eq, Show)



main :: IO ()
main = do
    (samples, instrs) <- readFile "inputs/day16" >>= parseInput
    let allOC = [minBound..] :: [OpCode]
    let code  = (\(c, _, _, _) -> c)

    let ocs  = map (\(b, a, i) -> (code i, S.fromList $ filter (\oc -> exec oc b i == a) allOC)) samples
    print $ length $ filter (\x -> (S.size $ snd x) >= 3) ocs

    let initVec = (V.replicate $ S.fromList allOC) :: V.Vector 16 (S.Set OpCode)
    let ocVec   = eliminate $ foldl (\v (c, s) -> v V.// [(c, (S.intersection (V.index v c) s))]) initVec ocs
    let regs    = foldl (\rs i -> exec (V.index ocVec (code i)) rs i) (V.replicate 0) instrs
    print $ V.index regs 0
    


eliminate :: V.Vector 16 (S.Set OpCode) -> V.Vector 16 OpCode
eliminate vec = fst $ until (V.and . V.map S.null . snd) go (initVec, vec) where
    initVec = V.replicate minBound
    go (ocv, v) = (ocv', v') where
        lst1 = map (\(c, s) -> (c, S.findMin s)) $ filter (\t -> S.size (snd t) == 1) (zip [0..15] $ V.toList v)
        set1 = foldl (\s (_, oc) -> S.insert oc s) S.empty lst1
        v'   = V.map (flip S.difference set1) v
        ocv' = ocv V.// lst1



exec :: OpCode -> Registers -> Instruction -> Registers
exec oc r (_, in1, in2, out) = r V.// [(out, result)] where
    regA = V.index r in1
    regB = V.index r in2
    valA = fromInteger $ getFinite in1
    valB = fromInteger $ getFinite in2
    result = case oc of
        ADDR -> regA  +  regB
        ADDI -> regA  +  valB
        MULR -> regA  *  regB
        MULI -> regA  *  valB
        BANR -> regA .&. regB
        BANI -> regA .&. valB
        BORR -> regA .|. regB
        BORI -> regA .|. valB
        SETR -> regA
        SETI -> valA
        GTIR -> if valA  > regB then 1 else 0
        GTRI -> if regA  > valB then 1 else 0
        GTRR -> if regA  > regB then 1 else 0
        EQIR -> if valA == regB then 1 else 0
        EQRI -> if regA == valB then 1 else 0
        EQRR -> if regA == regB then 1 else 0
    


parseInput :: String -> IO ([Sample], [Instruction])
parseInput raw = do
    case parse inputFile "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return x



inputFile :: Parser ([Sample], [Instruction])
inputFile = do
    samples <- many sample
    newline
    newline
    instrs <- many (instruction <* newline)
    return (samples, instrs)



sample :: Parser Sample
sample = do
    string "Before: "
    before <- registers
    newline
    ins <- instruction
    newline
    string "After:  "
    after <- registers
    newline
    newline
    return $ (before, after, ins)



registers :: Parser Registers
registers = do
    char '[' 
    regs <- (many digitChar) `sepBy` (string ", ")
    char ']' 
    return $ fromJust $ V.fromList $ map read regs



instruction :: Parser Instruction
instruction = do
    ins <- (many digitChar) `sepBy` (char ' ')
    let [code, in1, in2, out] = map read ins
    return (finite code, finite in1, finite in2, finite out)

