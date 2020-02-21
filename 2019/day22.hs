import           AOC.Common
import           Data.Foldable (foldl')
import           Data.List     (lookup, sort, zip)
import           Data.Tuple    (snd, swap)
import           Data.Void     (Void)
import           Text.Megaparsec      (Parsec, errorBundlePretty, many, oneOf, parse, (<|>))
import           Text.Megaparsec.Char (newline, string)


type Parser = Parsec Void String
type Deck = [Int]
type Shuffle = Deck -> Deck



main :: IO ()
main = do 
    shuffle <- readFile "inputs/day22" >>= parseInput

    let deck = shuffle [0..10006]

    case lookup 2019 $ zip deck [0..] of
        Nothing -> error "Not found!"
        Just x  -> print x
   

parseInput :: String -> IO Shuffle
parseInput raw = do
    case parse (many (newStack <|> cutCards <|> increment)) "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return $ foldl' (flip (.)) id x


newStack :: Parser Shuffle
newStack = do
    string "deal into new stack"
    newline
    return reverse

cutCards :: Parser Shuffle
cutCards = do
    string "cut "
    n <- read <$> many (oneOf "-0123456789")
    newline
    return (\deck -> uncurry (++) $ swap $ splitAt (n `mod` (length deck)) deck)

increment :: Parser Shuffle
increment = do
    string "deal with increment "
    n <- read <$> many (oneOf "0123456789")
    newline
    return (\deck -> map snd $ sort $ map (\(a, b) -> ((a * n) `mod` (length deck), b)) (zip [0..] deck))
