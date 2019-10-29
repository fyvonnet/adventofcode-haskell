import           Coord
import           Data.List
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Set as Set


type Parser = Parsec Void String



main :: IO ()
main = do
    (poss, vels) <- unzip <$> (readFile "inputs/day10" >>= parseInput)
    let (time, endPoss)  = moveStars 0 poss vels
    let starsSet = Set.fromList endPoss
    let (minx, miny, maxx, maxy) = limits endPoss
    let allCoords = map (\y -> [(Coord x y) | x <- [minx..maxx]]) [miny..maxy]
    putStr $ concat $ map (\l -> (++ "\n") $ map (\p -> if Set.member p starsSet then '*' else ' ') l) allCoords
    print time



moveStars tm poss vels 
    | maxy - miny == 9 = (tm, poss)
    | otherwise        = moveStars (tm + 1) poss' vels
    where
        poss' = map (uncurry addCoord) $ zip poss vels
        (_, miny, _, maxy) = limits poss



limits :: [Coord] -> (Int, Int, Int, Int)
limits cs = (minimum xs, minimum ys, maximum xs, maximum ys)
    where (xs, ys) = (map _x cs, map _y cs)
    


parseInput :: String -> IO [(Coord, Coord)]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser (Coord, Coord)
inputLine = do
    string "position=<"
    posx <- read <$> many coordChar
    string ", "
    posy <- read <$> many coordChar
    string "> velocity=<"
    velx <- read <$> many coordChar
    string ", "
    vely <- read <$> many coordChar
    string ">"
    eol
    return ((Coord posx posy), (Coord velx vely))



coordChar :: Parser Char
coordChar = char ' ' <|> char '-' <|> numberChar
