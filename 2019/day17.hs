import           AOC.Common
import           AOC.Coord
import           Control.Lens (_1, _2, over, set)
import           Data.Char (chr, ord) 
import           Data.Foldable (foldl')
import           Data.Set (Set)
import           IntCode
import qualified Data.Set as S


type Robot    = (Coord, AbsDirection)
type Scaffold = Set Coord
type Data     = (Scaffold, Robot)

data Movement = TURN RelDirection | FORWARD Int

instance Show Movement where
    show (TURN LEFT ) = "L"
    show (TURN RIGHT) = "R"
    show (FORWARD n ) = show n


main :: IO ()
main = do
    ics <- loadCode "inputs/day17"

    let (output, _)       = runIntCode [] ics
    let (scaffold, robot) = foldl makeData (S.empty, undefined) $ getTextMap $ map chr output
    print $ sum $ map (\(Coord x y) -> x * y) $ filter (intersection scaffold) $ S.toList scaffold

    -- Generate list of movements. Final program will be created manually.
    print $ turnRobot [] scaffold robot

    prog <- map ord <$> readFile "day17-prog"
    let (output2, _) = runIntCode prog $ writeMemory 0 2 ics
    print $ last output2


turnRobot :: [Movement] -> Scaffold -> Robot -> [Movement]
turnRobot ms s (c, f) = 
    case filter (\rd -> S.member (relNeighbour f rd c) s) [LEFT, RIGHT] of
        [d] -> forwardRobot ((TURN d):ms) s 0 (c, turn d f)
        [ ] -> reverse ms
        x   -> error $ show x


forwardRobot :: [Movement] -> Scaffold -> Int -> Robot -> [Movement]
forwardRobot ms s i (c, f)
    | S.member c' s = forwardRobot ms s (i + 1) (c', f)
    | otherwise     = turnRobot ((FORWARD i):ms) s (c, f)
    where c' = relNeighbour f FRONT c


makeData :: Data -> (Coord, Char) -> Data
makeData d (c, '#') = over _1 (S.insert c) d
makeData d (c, '^') = set  _2 (c, NORTH)   d
makeData d (c, '>') = set  _2 (c, EAST )   d
makeData d (c, 'v') = set  _2 (c, SOUTH)   d
makeData d (c, '<') = set  _2 (c, WEST )   d
makeData d  _       =                      d


intersection :: Scaffold -> Coord -> Bool
intersection s c = and $ map (flip S.member s . nextCoord c) allAbsDirections
