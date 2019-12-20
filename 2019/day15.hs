import           Data.Either (fromLeft, fromRight)
import           IntCode

data Robot = RB { _trail :: [Int], _ics :: ICState }


main :: IO ()
main = do
    ics <- loadCode "inputs/day15"
    let robot = fromLeft undefined $ navigate 0 [(RB [] ics)]
    print $ length $ _trail robot
    print $ fromRight undefined $ navigate 0 [(RB [] (_ics robot))]


navigate :: Int -> [Robot] -> Either Robot Int
navigate tm [] = Right tm
navigate tm (r:rs) = do
    let trail  = _trail r
    let direcs = if null trail then [1..4] else filter (/= (turnBack $ head trail)) [1..4]
    let tm' = max tm (length $ _trail r)
    case try [] r direcs of
        Left rbs -> navigate tm' (rs ++ rbs)
        Right r  -> Left r


try :: [Robot] -> Robot -> [Int] -> Either [Robot] Robot
try rbs _  []     = Left rbs
try rbs rb (d:ds)
    | output == 0 = try rbs rb ds
    | output == 1 = try (r:rbs) rb ds
    | output == 2 = Right r where
    r = (RB (d:_trail rb) ics')
    ([output], ics') = runIntCode [d] (_ics rb)


turnBack :: Int -> Int
turnBack 1 = 2
turnBack 2 = 1
turnBack 3 = 4
turnBack 4 = 3

