import           AOC.Coord
import           Data.Array
import           Data.List
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Data.Map as M



type Parser   = Parsec Void String
type SizeMap  = M.Map (Maybe Coord) Int



main :: IO ()
main = do
    input <- readFile "inputs/day06" >>= parseInput
    let grid = array ((Coord 0 0), (Coord gridMaxIndex gridMaxIndex)) [(c, closest input c) | c <- gridCoords]
    
    -- areas that reach the border of the grid are considered infinite
    let borderCoords    = concat [[(Coord 0 i), (Coord gridMaxIndex i), (Coord i 0), (Coord i gridMaxIndex)] | i <- gridIndices]
    let infiniteAreaSet = S.fromList $ filter isJust $ map ((!) grid) borderCoords
    let finiteAreaSet   = S.difference (S.fromList $ map Just input) infiniteAreaSet

    -- initialize size of finite areas since M.adjust will skip non-existing keys
    let sizeMap = M.fromList [(c, 0) | c <- S.toList finiteAreaSet]

    -- largest non-infinite area
    print $ maximum $ M.elems $ foldl (flip $ M.adjust (+1)) sizeMap $ elems grid

    -- size of the safe area
    print $ length $ filter (< 10000) $ map (\c -> sum $ map (taxicab c) input) gridCoords



-- find the coordinate closest to the reference coordinate
-- returns Nothing is several coordinates are closest
closest :: [Coord] -> Coord -> Maybe Coord
closest coords ref
    | fst smlDist == fst sndDist = Nothing
    | otherwise                  = Just $ snd smlDist
    where (smlDist:sndDist:_) = sort [(taxicab c ref, c) | c <- coords]



parseInput :: String -> IO [Coord]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser Coord
inputLine = do
    x <- read <$> many numberChar
    string ", "
    y <- read <$> many numberChar
    eol
    return (Coord x y)
        


gridSize :: Int
gridSize = 400

gridMaxIndex = (gridSize - 1)

gridIndices :: [Int]
gridIndices = [0..gridMaxIndex]

gridCoords :: [Coord]
gridCoords = [(Coord x y) | x <- gridIndices, y <- gridIndices]

