import           AOC.Coord
import           IntCode
import           Data.Ix (range)
import           Data.List (groupBy, sort)
import           Data.Map (Map)
import qualified Data.Map as M


data Direction = UP | RIGHT | DOWN | LEFT deriving (Show, Enum)
data RobotState = RS 
    { ics   :: ICState
    , dir   :: Direction
    , coord :: Coord
    , hull  :: Map Coord Int } deriving Show



main :: IO ()
main = do
    ics <- loadCode "inputs/day11"
    let (output, ics') = runIntCode [0] ics
    print $ M.size $ hull $ until donePainting step (RS ics UP (Coord 0 0) M.empty)
    let h = hull $ until donePainting step (RS ics UP (Coord 0 0) (M.singleton (Coord 0 0) 1))
    let k = M.keys h
    putStrLn $ unlines $ map (map (setPixel h)) $ groupByLine $ sort $ range (head k, last k)

groupByLine :: [Coord] -> [[Coord]]
groupByLine = groupBy (\(Coord _ y0) (Coord _ y1) -> y0 == y1)

setPixel :: Map Coord Int -> Coord -> Char
setPixel m c = if M.findWithDefault 0 c m == 0 then ' ' else '#'

step :: RobotState -> RobotState
step (RS ics dir coord hull) = (RS ics' dir' coord' hull') where
    color = M.findWithDefault 0 coord hull
    ([color', t], ics') = runIntCode [color] ics
    dir' = turn t dir
    coord' = move dir' coord
    hull' = M.insert coord color' hull


donePainting :: RobotState -> Bool
donePainting = not . isRunning . ics 

turn :: Int -> Direction -> Direction
turn 0 UP   = LEFT
turn 0 d    = pred d
turn 1 LEFT = UP
turn 1 d    = succ d

move :: Direction -> Coord -> Coord
move DOWN  (Coord x y) = (Coord  x     (y + 1))
move RIGHT (Coord x y) = (Coord (x + 1) y     )
move UP    (Coord x y) = (Coord  x     (y - 1))
move LEFT  (Coord x y) = (Coord (x - 1) y     )
