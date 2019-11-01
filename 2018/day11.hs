import           AOC.Coord
import           Control.Monad.Reader
import           Data.Array
import           Data.Ix
import           Data.List
import           Data.Maybe
import qualified Data.Map as M


type SatMatrix = M.Map Coord Int
data Square = Square Coord Int Int deriving Show

instance Ord Square where
    compare (Square _ _ pa) (Square _ _ pb) = compare pa pb

instance Eq Square where
    (==) (Square _ _ pa) (Square _ _ pb) = (==) pa pb




main :: IO ()
main = do
    let satSquare = foldl insertSquare M.empty $ range ((Coord 1 1), (Coord 300 300))

    let (Square (Coord x1 y1) _ _) = foldl1 (findAll satSquare) $ squareCoords 3
    putStrLn $ show x1 ++ "," ++ show y1

    let (Square (Coord x2 y2) s _) = foldl1 (findAll satSquare) [sq | s <- [1..300], sq <- squareCoords s]
    putStrLn $ show x2 ++ "," ++ show y2  ++ "," ++ show s



insertSquare :: SatMatrix -> Coord -> SatMatrix
insertSquare sat c = M.insert c power sat
    where
        (Coord x y) = c
        power = (cellPower c) + (lu (Coord (x-1) y) sat) + (lu (Coord x (y-1)) sat) - (lu (Coord (x-1) (y-1)) sat)



findAll :: SatMatrix -> Square -> Square -> Square
findAll sat sq (Square c s _) = max sq $ Square c s $ squarePower sat c s



squarePower :: SatMatrix -> Coord -> Int -> Int
squarePower sat (Coord x y) s =
        (lu (Coord (x-1) (y-1)) sat) - (lu (Coord (x+l) (y-1)) sat) - (lu (Coord (x-1) (y+l)) sat) + (lu (Coord (x+l) (y+l)) sat)
    where l = s - 1


lu = M.findWithDefault 0
        
    


cellPower :: Coord -> Int
cellPower (Coord x y) = do
    let rackID = x + 10
    (((rackID * y + serialNum) * rackID) `quot` 100 `mod` 10) - 5



squareCoords :: Int -> [Square]
squareCoords size = map (\c -> Square c size 0) $ range ((Coord 1 1), (Coord l l))
    where l = (300 - size + 1)



serialNum :: Int
serialNum = 8141
