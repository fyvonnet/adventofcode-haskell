import           AOC.Coord
import           Data.Array
import           Data.List
import qualified Data.Map as M


type SatMap   = M.Map Coord Int
type SatArray = Array Coord Int
data Square   = Square Coord Int Int deriving Show

instance Ord Square where
    compare (Square _ _ pa) (Square _ _ pb) = compare pa pb

instance Eq Square where
    (==) (Square _ _ pa) (Square _ _ pb) = (==) pa pb



main :: IO ()
main = do
    serialNum <- read <$> readFile "inputs/day11"

    let satMap    = foldl (insertSquare serialNum) M.empty $ range ((Coord 1 1), (Coord 300 300))
    let zeroArray = listArray ((Coord 0 0), (Coord 300 300)) $ repeat 0
    let satArray  = zeroArray // M.toList satMap

    let (Square (Coord x1 y1) _ _) = foldl1 (findAll satArray) $ squareCoords 3
    putStrLn $ show x1 ++ "," ++ show y1

    let (Square (Coord x2 y2) s _) = foldl1 (findAll satArray) [sq | s <- [1..300], sq <- squareCoords s]
    putStrLn $ show x2 ++ "," ++ show y2  ++ "," ++ show s



insertSquare :: Int -> SatMap -> Coord -> SatMap
insertSquare sn sat c = M.insert c power sat
    where
        (Coord x y) = c
        power =
            (cellPower sn c) +
            (M.findWithDefault 0 (Coord (x - 1)  y     ) sat) +
            (M.findWithDefault 0 (Coord  x      (y - 1)) sat) -
            (M.findWithDefault 0 (Coord (x - 1) (y - 1)) sat)



findAll :: SatArray -> Square -> Square -> Square
findAll sat sq (Square c s _) = max sq $ Square c s power
    where
        (Coord x y) = c
        power =
            (sat ! (Coord (x     - 1) (y     - 1))) -
            (sat ! (Coord (x + s - 1) (y     - 1))) -
            (sat ! (Coord (x     - 1) (y + s - 1))) +
            (sat ! (Coord (x + s - 1) (y + s - 1)))

        

cellPower :: Int -> Coord -> Int
cellPower sn (Coord x y) = (((rid * y + sn) * rid) `quot` 100 `mod` 10) - 5
    where rid = x + 10



squareCoords :: Int -> [Square]
squareCoords size = map (\c -> Square c size 0) $ range ((Coord 1 1), (Coord l l))
    where l = (300 - size + 1)
