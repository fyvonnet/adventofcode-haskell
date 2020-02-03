import           AOC.Common
import           Data.Foldable (foldl')
import           Data.List     (find)
import           Data.Maybe    (fromJust)
import           Data.Tuple    (swap)
import           Data.Vector   (Vector, (//))
import           Data.Void     (Void)
import           Text.Megaparsec      (Parsec, errorBundlePretty, many, oneOf, parse, (<|>))
import           Text.Megaparsec.Char (newline, string)
import qualified Data.Vector as V


type Parser = Parsec Void String
data Technique = NEWSTACK | CUTCARDS Int | INCREMENT Int deriving Eq


main :: IO ()
main = do 
    input <- readFile "inputs/day22" >>= parseInput

    let deck = foldl' applyTechnique [0..10006] input
    print $ fst $ fromJust $ find ((== 2019) . snd) $ zip [0..] deck
   
    --let deck2 = [0..119315717514046]
    --print $ findRepeat $ iterate (\d -> foldl' applyTechnique d input) deck2


applyTechnique :: [Int] -> Technique -> [Int]

applyTechnique deck NEWSTACK = reverse deck

applyTechnique deck (CUTCARDS n) = uncurry (++) $ swap $ splitAt n' deck
    where n' = if n > 0 then n else (length deck) + n

applyTechnique deck (INCREMENT n) = go deck 0 (V.replicate len undefined) where
    len = length deck
    go []     _ vec = V.toList vec
    go (x:xs) r vec = go xs ((r + n) `mod` len) (vec // [(r, x)])


parseInput :: String -> IO [Technique]
parseInput raw = do
    case parse inputFile "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x


inputFile :: Parser [Technique]
inputFile = many (newStack <|> cutCards <|> increment)

newStack :: Parser Technique
newStack = do
    string "deal into new stack"
    newline
    return NEWSTACK

cutCards :: Parser Technique
cutCards = do
    string "cut "
    n <- read <$> many (oneOf "-0123456789")
    newline
    return $ CUTCARDS n

increment :: Parser Technique
increment = do
    string "deal with increment "
    n <- read <$> many (oneOf "0123456789")
    newline
    return $ INCREMENT n
