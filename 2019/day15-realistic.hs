{-# LANGUAGE TemplateHaskell #-}


import           AOC.Coord
import           Control.Lens
import           Data.Map (Map)
import           Data.Set (Set)
import           IntCode
import qualified Data.Map as Map
import qualified Data.Set as Set


data Square = WALL | PASSAGE [AbsDirection] deriving (Show, Eq)
type Area   = Map Coord Square

data RobotState = RobotState
    { _coord  :: Coord
    , _trail  :: [AbsDirection]
    , _ics    :: ICState
    , _nsteps :: Int
    , _oxy    :: Coord
    , _area   :: Area
    }

makeLenses ''RobotState


main :: IO ()
main = do
    ics <- loadCode "inputs/day15"

    let rs = explore RobotState { 
        _coord  = (Coord 0 0) ,
        _trail  = [] ,
        _ics    = ics ,
        _nsteps = undefined ,
        _oxy    = undefined ,
        _area   = Map.singleton (Coord 0 0) (PASSAGE allAbsDirections) }

    print $ rs^.nsteps
    print $ expand (Set.fromList $ Map.keys $ Map.filter (== WALL) $ rs^.area) [(1, rs^.oxy)] 0


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

        (_, _, [0]) -> explore $ rs'
            & area %~ Map.insert (absNeighbour (head av) (rs^.coord)) WALL

        (_, _, [1]) -> explore   rs_moved
        
        (_, _, [2]) -> explore $ rs_moved
            & nsteps .~ (length $ rs^.trail) + 1
            & oxy    .~ rs^.coord

    where
        (output, ics') = runIntCode (dir2cmd $ head av) (rs^.ics)
        (PASSAGE av)   = Map.findWithDefault defAvail (rs^.coord) (rs^.area)
        defAvail = PASSAGE $ filter (\d -> d /= turnBack) allAbsDirections
        turnBack = turn BACK $ head (rs^.trail)
        rs'      = rs & area %~ Map.insert (rs^.coord) (PASSAGE $ tail av)
        rs_moved = rs' 
            & ics   .~ ics'
            & coord %~ absNeighbour (head av)
            & trail %~ (:) (head av)


dir2cmd :: AbsDirection -> [Int]
dir2cmd NORTH = [1]
dir2cmd SOUTH = [2]
dir2cmd WEST  = [3]
dir2cmd EAST  = [4]
