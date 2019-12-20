import           AOC.Coord
import           Data.Either (fromLeft)
import           IntCode

data Robot = RB { _trail :: [Int], _ics :: ICState } deriving (Show)


main :: IO ()
main = do
    ics <- loadCode "inputs/day15"
    let robot = navigate [(RB [] ics)]
    print $ length $ _trail robot
    print $ propagate 0 [(RB [] (_ics robot))]


propagate :: Int -> [Robot] -> Int
propagate tm [] = tm
propagate tm (r:rs) = propagate tm' (rs ++ rbs) where
    trail  = _trail r
    direcs = if null trail then [1..4] else filter (/= (moveBack $ head trail)) [1..4]
    tm' = max tm (length $ _trail r)
    rbs = fromLeft undefined $ try [] r direcs


navigate :: [Robot] -> Robot
navigate (r:rs) = do
    let trail  = _trail r
    let direcs = if null trail then [1..4] else filter (/= (moveBack $ head trail)) [1..4]
    case try [] r direcs of
        Left rbs -> navigate (rs ++ rbs)
        Right r  -> r


try :: [Robot] -> Robot -> [Int] -> Either [Robot] Robot
try rbs _  []     = Left rbs
try rbs rb (d:ds)
    | output == 0 = try rbs rb ds
    | output == 1 = try (robot':rbs) rb ds
    | output == 2 = Right robot' where
    robot' = (RB (d:_trail rb) ics')
    ([output], ics') = runIntCode [d] (_ics rb)


moveBack :: Int -> Int
moveBack 1 = 2
moveBack 2 = 1
moveBack 3 = 4
moveBack 4 = 3

