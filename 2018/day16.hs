import           Data.Bits
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V


type Parser      = Parsec Void String
type Registers   = V.Vector Int
type Sample      = (Registers, Registers, Instruction)
type Instruction = (Int, Int, Int, Int)

data OpCode = ADDR | ADDI |
              MULR | MULI |
              BANR | BANI |
              BORR | BORI |
              SETR | SETI |
              GTIR | GTRI | GTRR |
              EQIR | EQRI | EQRR
              deriving (Enum, Bounded, Ord, Eq)



main :: IO ()
main = do
    (samples, instrs) <- readFile "inputs/day16" >>= parseInput
    let allOC = [minBound..] :: [OpCode]
    let code  = (\(c, _, _, _) -> c)

    let ocs  = map (\(b, a, i) -> (code i, S.fromList $ filter (\oc -> run oc b i == a) allOC)) samples
    print $ length $ filter (\x -> (S.size $ snd x) >= 3) ocs

    let initVec = V.replicate 16 $ S.fromList allOC
    let x       = foldl (\v (c, s) -> v V.// [(c, (S.intersection (v V.! c) s))]) initVec ocs
    let ocVec   = V.fromList $ M.elems $ snd $ until (null . fst) eliminate (zip [0..] $ V.toList x, M.empty)
    let regs    = foldl (\rs i -> run (ocVec V.! (code i)) rs i) (V.fromList [0,0,0,0]) instrs
    print $ regs V.! 0
    
   

eliminate :: ([(Int, S.Set OpCode)], M.Map Int OpCode) -> ([(Int, S.Set OpCode)], M.Map Int OpCode)
eliminate (lst, m) = (lst', m') where
    lst1 = filter (\(_, s) -> S.size s == 1) lst
    set1 = S.unions $ map snd lst1
    m'   = foldl (\m (c, s) -> M.insert c (S.findMin s) m) m lst1
    lst' = filter (not . S.null . snd) $ map (\(c, s) -> (c, S.difference s set1)) lst



run :: OpCode -> Registers -> Instruction -> Registers
run oc r (_, valA, valB, out) = r V.// [(out, result)] where
    regA = r V.! valA
    regB = r V.! valB
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
    return $ V.fromList $ map read regs



instruction :: Parser Instruction
instruction = do
    ins <- (many digitChar) `sepBy` (char ' ')
    let [code, in1, in2, out] = map read ins
    return (code, in1, in2, out)
