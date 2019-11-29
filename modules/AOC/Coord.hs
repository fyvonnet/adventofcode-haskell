module AOC.Coord where

import Data.Array

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
