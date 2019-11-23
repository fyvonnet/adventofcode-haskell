{-# LANGUAGE TemplateHaskell #-}

import           AOC.Coord
import           Control.Lens (makeLenses, over, use, view, (%=))
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Ix
import           Data.Void
import           Data.Sequence (ViewL((:<)), (<|))
import           GHC.Exts
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Data.Map      as M
import qualified Data.Sequence as S

type Parser  = Parsec Void String
type Ground  = M.Map Coord Square
type Queue   = S.Seq Coord
data Square  = SAND | CLAY | FLOW | WATER deriving (Eq, Ord)
data Flow    = Blocked Coord | Overflow Coord
data MyState = MyState { _ground :: Ground, _bottom :: Int }

makeLenses ''MyState
makeLenses ''Coord



main :: IO ()
main = do
    input <- concat <$> (readFile "inputs/day17" >>= parseInput)

    let ys      = map _y input
    let top     = minimum ys
    let bottom  = maximum ys
    let start   = S.singleton (Coord 500 top)
    let initMap = M.fromList $ zip input $ repeat CLAY
    let sqMap   = view ground $ execState (iterateUntilM S.null run start) (MyState initMap bottom)
    let res     = foldl counter (0, 0) (M.elems sqMap)

    print $ fst res
    print $ snd res



counter (a, b) FLOW  = (a + 1, b    )
counter (a, b) WATER = (a + 1, b + 1)
counter (a, b) _     = (a,     b    )



run :: Queue -> State MyState Queue
run queue = do
    let (c :< cs) = S.viewl queue
    c' <- downwardFlow c
    case c' of
        Nothing -> return cs
        Just x  -> do
            newcs <- fillReservoir x
            return $ foldl (flip (<|)) cs newcs



downwardFlow :: Coord -> State MyState (Maybe Coord)
downwardFlow c = do
    ground %= (M.insert c FLOW)
    b <- use bottom
    let low = lower c
    if _y low > b then return Nothing else do
        square <- getSquareS low 
        case square of
            SAND      -> downwardFlow low
            FLOW      -> return Nothing
            otherwise -> return $ Just c



horizontalFlow :: Int -> Coord -> State MyState Flow
horizontalFlow m coord = do
    ground %= M.insert coord FLOW
    let coord' = (over x (+ m)) coord
    lowsq <- getSquareS $ lower coord
    nxtsq <- getSquareS coord'
    if (lowsq /= CLAY) && (lowsq /= WATER) then return (Overflow coord)
    else if nxtsq == CLAY then return (Blocked coord)
    else horizontalFlow m coord'



fillReservoir :: Coord -> State MyState [Coord]
fillReservoir c = do
    res1 <- horizontalFlow (-1) c -- flow to the left
    res2 <- horizontalFlow   1  c -- flow to the right
    case (res1, res2) of
        (Overflow c1, Blocked  _ ) -> return [c1]
        (Blocked  _ , Overflow c2) -> return [c2]
        (Overflow c1, Overflow c2) -> return [c1, c2]
        (Blocked  c1, Blocked  c2) -> do
            ground %= (M.union $ foldl (\m c -> M.insert c WATER m) M.empty $ range (c1, c2))
            fillReservoir ((over y pred) c)



lower :: Coord -> Coord
lower = over y succ



getSquare :: Ground -> Coord -> Square
getSquare m c = M.findWithDefault SAND c m



getSquareS :: Coord -> State MyState Square
getSquareS c = do
    m <- use ground
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
