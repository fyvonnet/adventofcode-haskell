{-# LANGUAGE TemplateHaskell #-}


import           AOC.Coord
import           Control.Lens ((&), (%~), (+~), (-~), (.~), (^.), makeLenses)
import           Data.Set (Set)
import           IntCode
import qualified Data.Set as Set


data Move   = FW AbsDirection | BK AbsDirection 
data RobotState = RobotState
    { _coord   :: Coord
    , _ics     :: ICState
    , _steps   :: Int
    , _oxy_stp :: Int
    , _oxy_crd :: Coord
    , _walls   :: Set Coord
    }

makeLenses ''RobotState


main :: IO ()
main = do
    ics <- loadCode "inputs/day15"

    let rs = explore (map FW allAbsDirections) RobotState { 
        _coord   = (Coord 0 0) ,
        _ics     = ics         ,
        _steps   = 1           ,
        _oxy_stp = undefined   ,
        _oxy_crd = undefined   ,
        _walls   = Set.empty   }

    print (rs^.oxy_stp)
    print $ fill (rs^.walls) [(1, rs^.oxy_crd)] 0


explore :: [Move] -> RobotState -> RobotState

explore []             rs = rs

explore ((BK dir):mvs) rs = explore mvs $ rs
    & steps -~ 1
    & coord %~ absNeighbour dir
    & ics   %~ snd . runIntCode (dir2cmd dir)

explore ((FW dir):mvs) rs = case output of
    0 -> explore mvs  rs  & walls %~ Set.insert (absNeighbour dir (rs^.coord))
    1 -> explore mvs' rs'
    2 -> explore mvs' rs' & (oxy_stp .~ rs^.steps) & (oxy_crd .~ rs^.coord)
    where
        ([output], ics') = runIntCode (dir2cmd dir) (rs^.ics)
        turnBack = turn BACK dir
        mvs' = [FW d | d <- allAbsDirections, d /= turnBack] ++ [BK turnBack] ++ mvs
        rs'  = rs
            & steps +~ 1
            & coord %~ absNeighbour dir
            & ics   .~ ics'


fill :: Set Coord -> [(Int, Coord)] -> Int -> Int
fill _       []              time = time
fill blocked ((t, coord):qs) time = fill blocked' queue t
    where
        blocked' = Set.insert coord blocked
        queue    = qs ++ [(t + 1, c) | c <- allNextCoords coord, Set.notMember c blocked]


dir2cmd :: AbsDirection -> [Int]
dir2cmd NORTH = [1]
dir2cmd SOUTH = [2]
dir2cmd WEST  = [3]
dir2cmd EAST  = [4]
