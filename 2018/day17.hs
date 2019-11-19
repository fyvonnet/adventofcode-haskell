import           AOC.Coord
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Ix
import           Data.Void
import           GHC.Exts
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Data.Map      as M
import qualified Data.Sequence as S



type Parser  = Parsec Void String
type Ground  = M.Map Coord Square
type MyState = State Ground
type Queue   = S.Seq Coord
data Square  = SAND | CLAY | FLOW | WATER deriving (Eq, Ord)
data Flow    = Blocked Coord | Overflow Coord



main :: IO ()
main = do
    input <- concat <$> (readFile "inputs/day17" >>= parseInput)
    let start   = S.singleton (Coord 500 0)
    let initMap = M.fromList $ zip input $ repeat CLAY
    let sqMap   = execState (iterateUntilM S.null run start) initMap

    let xs     = map _x $ M.keys sqMap
    let ys     = map _y input
    let coords = [(Coord x y) | y <- [minimum ys..maximum ys], x <- [minimum xs..maximum xs]]
    let res    = foldl counter (0, 0) $ map (getSquare sqMap) coords

    print $ fst res
    print $ snd res



counter (a, b) FLOW  = (a + 1, b    )
counter (a, b) WATER = (a + 1, b + 1)
counter (a, b) _     = (a,     b    )



run :: Queue -> MyState Queue
run queue = do
    let (c S.:< cs) = S.viewl queue
    c' <- flowDown c
    case c' of
        Nothing -> return cs
        Just x  -> do
            newcs <- fillReservoir x
            return $ foldl (flip (S.<|)) cs newcs



flowDown :: Coord -> MyState (Maybe Coord)
flowDown c = do
    modify (M.insert c FLOW)
    let low = lower c
    if _y low > 2000 then return Nothing else do
        square <- getSquareS low 
        case square of
            SAND      -> flowDown low
            FLOW      -> return Nothing
            otherwise -> return $ Just c



flowSide :: (Coord -> Coord) -> Coord -> MyState Flow
flowSide f coord = do
    modify (M.insert coord FLOW)
    let nxtcrd = f coord
    lowsq <- getSquareS $ lower coord
    nxtsq <- getSquareS nxtcrd
    if (lowsq /= CLAY) && (lowsq /= WATER) then return (Overflow coord)
    else if nxtsq == CLAY then return (Blocked coord)
    else flowSide f nxtcrd



fillReservoir :: Coord -> MyState [Coord]
fillReservoir c = do
    res1 <- flowSide left  c
    res2 <- flowSide right c
    case (res1, res2) of
        (Overflow c1, Blocked  _ ) -> return [c1]
        (Blocked  _ , Overflow c2) -> return [c2]
        (Overflow c1, Overflow c2) -> return [c1, c2]
        (Blocked  c1, Blocked  c2) -> do
            modify (M.union $ foldl (\m c -> M.insert c WATER m) M.empty $ range (c1, c2))
            fillReservoir (upper c)



upper (Coord c r) = (Coord c (r - 1))
lower (Coord c r) = (Coord c (r + 1))
left  (Coord c r) = (Coord (c - 1) r)
right (Coord c r) = (Coord (c + 1) r)



getSquare :: Ground -> Coord -> Square
getSquare m c = M.findWithDefault SAND c m



getSquareS :: Coord -> MyState Square
getSquareS c = do
    m <- get
    return $ getSquare m c



parseInput :: String -> IO [[Coord]]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser [Coord]
inputLine = do
    xy <- oneOf "xy"
    char '='
    a <- read <$> many numberChar
    string ", "
    oneOf "xy"
    char '='
    b <- read <$> many numberChar
    string ".."
    c <- read <$> many numberChar
    eol

    case xy of
        'x' -> return [(Coord a y) | y <- [b..c]]
        'y' -> return [(Coord x a) | x <- [b..c]]
