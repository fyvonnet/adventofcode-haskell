import           AOC.Coord
import           Control.Monad.Reader
import           Data.Array
import           Data.List
import           Data.Maybe
import qualified Data.Map as M


type PowerArray  = Array Coord Int
type SearchState = (PowerArray, Maybe Square)
data Square = Square Coord (Maybe Int) Int

instance Ord Square where
    compare (Square _ _ pa) (Square _ _ pb) = compare pa pb

instance Eq Square where
    (==) (Square _ _ pa) (Square _ _ pb) = (==) pa pb

instance Show Square where
    show (Square (Coord x y) Nothing   _) = show x ++ "," ++ show y 
    show (Square (Coord x y) (Just s)  _) = show x ++ "," ++ show y ++ "," ++ show s
        



main :: IO ()
main = do
    let arrayCoords = squareCoords 1
    let cellArray = array ((Coord 1 1), (Coord 300 300)) $ zip arrayCoords $ map cellPower arrayCoords

    let (Just result1) = foldl (searchMax3 cellArray) Nothing $ squareCoords 3
    putStrLn $ show $ result1

    let (_, Just result2) = runReader (foldM searchMax (cellArray, Nothing) [1..300]) cellArray
    putStrLn $ show $ result2
    



searchMax3 :: PowerArray -> Maybe Square -> Coord -> Maybe Square
searchMax3 ca sqmax coord = sqmax'
    where
        (Coord cx cy) = coord
        square        = (Square coord Nothing pwr)
        sqmax'        = if isNothing sqmax then Just square else max square <$> sqmax
        coords        = [(Coord x y) | x <- [cx..cx+2], y <- [cy..cy+2]]
        pwr           = sum $ map ((!) ca) coords



searchMax :: SearchState -> Int -> Reader PowerArray SearchState
searchMax (pa, sqmax) size = do
    let sqcoords =  squareCoords size
    squares      <- mapM (makeSquare pa size) sqcoords
    let lmax     =  maximum squares
    let sqmax'   =  if isNothing sqmax then Just lmax else max lmax <$> sqmax
    let pa'      =  array (head sqcoords, last sqcoords) [(c, p) | (Square c _ p) <- squares]
    return $ (pa', sqmax')



makeSquare :: PowerArray -> Int -> Coord -> Reader PowerArray Square
makeSquare pa size coord = do 
    ca <- ask
    let (Coord cx cy) = coord
    let l = size - 1
    let brdCoords = if size == 1 then [] else (Coord (cx + l) (cy + l)) : [c | m <- [0..l-1], c <- [(Coord (cx + l) (cy + m)), (Coord (cx + m) (cy + l))]]
    let power = (pa ! coord) + sum (map ((!) ca) brdCoords)
    return (Square coord (Just size) power)



cellPower :: Coord -> Int
cellPower (Coord x y) = do
    let rackID = x + 10
    ((((rackID * y) + serialNum) * rackID) `quot` 100 `mod` 10) - 5



squareCoords :: Int -> [Coord]
squareCoords size = [(Coord x y) | y <- range, x <- range]
    where range = [1..(300 - size + 1)]



serialNum :: Int
serialNum = 8141
