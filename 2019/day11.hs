import           AOC.Coord
import           IntCode
import           Data.Ix (range)
import           Data.List (groupBy, sort)
import           Data.Map (Map)
import qualified Data.Map as M


type Hull = Map Coord Int
data Direction = UP | RIGHT | DOWN | LEFT deriving (Show, Enum)
data RobotState = RS 
    { ics   :: ICState
    , dir   :: Direction
    , coord :: Coord
    , hull  :: Hull } deriving Show



main :: IO ()
main = do
    ics <- loadCode "inputs/day11"
    let [h1, h2] = map (paintHull ics) [M.empty, (M.singleton (Coord 0 0) 1)]
    print $ M.size h1
    let k = M.keys h2
    putStrLn $ unlines $ map (map (displayPanel h2)) $ groupByLine $ sort $ range (head k, last k)


paintHull :: ICState -> Hull -> Hull
paintHull i h = hull $ until donePainting paintPanel (RS i UP (Coord 0 0) h)


groupByLine :: [Coord] -> [[Coord]]
groupByLine = groupBy (\(Coord _ y0) (Coord _ y1) -> y0 == y1)


displayPanel :: Hull -> Coord -> Char
displayPanel m c = if M.findWithDefault 0 c m == 0 then ' ' else '#'


paintPanel :: RobotState -> RobotState
paintPanel (RS ics dir coord hull) = (RS ics' dir' coord' hull') where
    ([color', t], ics') = runIntCode [color] ics
    color  = M.findWithDefault 0 coord hull
    dir'   = turn t dir
    coord' = move dir' coord
    hull'  = M.insert coord color' hull


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
