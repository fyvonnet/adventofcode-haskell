
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Map as M



type Parser   = Parsec Void String
type Coord    = (Int, Int)
type CountMap = M.Map Coord Int
data Claim    = Claim
    { num :: Int        -- claim number
    , coords :: [Coord] -- coordinates of all claimed squares
    }



main :: IO ()
main = do
    claims <- readFile "inputs/day03" >>= parseInput
    let countMap = foldl (M.unionWith (+)) M.empty (map (\c -> M.fromList [(crd, 1) | crd <- coords c]) claims)
    print $ length $ filter (>1) $ M.elems countMap
    print $ findNoOverlaps claims countMap



findNoOverlaps :: [Claim] -> CountMap -> Int
findNoOverlaps []     _ = error "Can't find claim without overlap"
findNoOverlaps (c:cs) m 
    | and (map ((== 1) . ((M.!) m)) $ coords c) = num c
    | otherwise = findNoOverlaps cs m



parseInput :: String -> IO [Claim]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser Claim
inputLine = do
    char '#'
    num    <- read <$> many numberChar
    string " @ "
    cx     <- read <$> many numberChar
    char ','
    cy     <- read <$> many numberChar
    string ": "
    width  <- read <$> many numberChar
    char 'x'
    height <- read <$> many numberChar
    eol
    return (Claim num [(x, y) | x <- [cx..(cx + width - 1)], y <- [cy..(cy + height - 1)]])

