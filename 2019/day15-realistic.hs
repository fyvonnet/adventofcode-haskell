{-# LANGUAGE TemplateHaskell #-}


import           AOC.Coord
import           Control.Lens
import           Data.Map (Map)
import           Data.Set (Set)
import           IntCode
import qualified Data.Map as Map
import qualified Data.Set as Set


data RobotState = RobotState
    { _coord  :: Coord
    , _trail  :: [AbsDirection]
    , _ics    :: ICState
    , _walls  :: Set Coord
    , _nsteps :: Int
    , _oxy    :: Coord
    , _avail  :: Map Coord [AbsDirection]
    }

makeLenses ''RobotState


main :: IO ()
main = do
    ics <- loadCode "inputs/day15"

    let rs = explore RobotState { 
        _coord  = (Coord 0 0) ,
        _trail  = [] ,
        _ics    = ics ,
        _walls  = Set.empty ,
        _nsteps = undefined ,
        _oxy    = undefined ,
        _avail  = Map.singleton (Coord 0 0) allAbsDirections }

    print $ rs^.nsteps

    print $ expand (rs^.walls) [(1, rs^.oxy)] 0


expand :: Set Coord -> [(Int, Coord)] -> Int -> Int
expand _       []    time = time
expand blocked ((t, coord):qs) time = expand blocked' queue t
    where
        blocked' = Set.insert coord blocked
        queue    = qs ++ [(t + 1, c) | c <- allNextCoords coord, Set.notMember c blocked]


explore :: RobotState -> RobotState
explore rs = 

    case (av, rs^.trail, output) of

        ([], [], _) -> rs

        ([],  _, _) -> explore $ rs
            & coord %~ absNeighbour turnBack
            & ics   %~ snd . runIntCode (dir2cmd turnBack)
            & trail %~ tail

        (_, _, [0]) -> explore $ rs
            & walls %~ Set.insert (absNeighbour (head av) (rs^.coord))
            & avail %~ Map.insert (rs^.coord) (tail av)

        (_, _, [1]) -> explore   rs_moved
        
        (_, _, [2]) -> explore $ rs_moved
            & nsteps .~ (length $ rs^.trail) + 1
            & oxy    .~ rs^.coord

    where
        (output, ics') = runIntCode (dir2cmd $ head av) (rs^.ics)
        av       = Map.findWithDefault defAvail (rs^.coord) (rs^.avail)
        defAvail = filter (\d -> d /= turnBack) allAbsDirections
        turnBack = turn BACK $ head (rs^.trail)
        rs_moved = rs 
            & ics   .~ ics'
            & coord %~ absNeighbour (head av)
            & avail %~ Map.insert (rs^.coord) (tail av)
            & trail %~ (:) (head av)


dir2cmd :: AbsDirection -> [Int]
dir2cmd NORTH = [1]
dir2cmd SOUTH = [2]
dir2cmd WEST  = [3]
dir2cmd EAST  = [4]
