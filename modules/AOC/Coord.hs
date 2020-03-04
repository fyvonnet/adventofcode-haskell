module AOC.Coord where

import Data.Array

data AbsDirection = NORTH | EAST | SOUTH | WEST deriving (Show, Eq, Enum, Ord, Bounded)
data RelDirection = FRONT | RIGHT | BACK | LEFT deriving (Show, Eq, Enum, Ord, Bounded)

data Coord = Coord {_x :: Int, _y :: Int } deriving Ix
--data Coord = Coord {_x :: Int, _y :: Int } deriving (Show, Ix)

instance Eq Coord where
    (==) (Coord ax ay) (Coord bx by) = (ax, ay) == (bx, by)
    
instance Ord Coord where
    compare (Coord ax ay) (Coord bx by) = compare (ay, ax) (by, bx)

instance Show Coord where
    show (Coord x y) = show (x, y)


showCoord (Coord x y) = show x ++ "," ++ show y

addCoord :: Coord -> Coord -> Coord
addCoord (Coord ax ay) (Coord bx by) = (Coord (ax + bx) (ay + by))

taxicab :: Coord -> Coord -> Int
taxicab (Coord ax ay) (Coord bx by) = (abs (ax - bx)) + (abs (ay - by))


-- to be removed
nextCoord :: Coord -> AbsDirection -> Coord
nextCoord (Coord x y) WEST  = (Coord (x - 1) y)
nextCoord (Coord x y) EAST  = (Coord (x + 1) y)
nextCoord (Coord x y) SOUTH = (Coord x (y + 1))
nextCoord (Coord x y) NORTH = (Coord x (y - 1))

absNeighbour ::  AbsDirection -> Coord -> Coord 
absNeighbour WEST  (Coord x y) = (Coord (x - 1) y)
absNeighbour EAST  (Coord x y) = (Coord (x + 1) y)
absNeighbour SOUTH (Coord x y) = (Coord x (y + 1))
absNeighbour NORTH (Coord x y) = (Coord x (y - 1))

relNeighbour :: AbsDirection -> RelDirection -> Coord -> Coord
relNeighbour a r c = absNeighbour (turn r a) c


allAbsDirections :: [AbsDirection]
allAbsDirections = [minBound..]


-- to be removed
allNextCoords :: Coord -> [Coord] 
allNextCoords c = map (nextCoord c) allAbsDirections


allNeighbours :: Coord -> [Coord] 
allNeighbours c = map (\d -> absNeighbour d c) allAbsDirections


turn :: RelDirection -> AbsDirection -> AbsDirection

turn FRONT ad   = ad

turn BACK WEST  = EAST
turn BACK EAST  = WEST
turn BACK SOUTH = NORTH
turn BACK NORTH = SOUTH

turn RIGHT WEST = NORTH
turn RIGHT ad   = succ ad

turn LEFT NORTH = WEST
turn LEFT ad    = pred ad

