import           AOC.Coord
import           Data.List (sortBy)
import           Data.Map (Map)
import           Data.Tuple (swap)
import qualified Data.Map as Map


main :: IO ()
main = do
    let input = 368078
    let (coords, dist) = spiral input Map.empty EAST (Coord 0 0)

    print dist
    print $ findSum input (Map.singleton (Coord 0 0) 1) coords


spiral :: Int -> Map Coord Int -> AbsDirection -> Coord -> ([Coord], Int)
spiral n m d c 
    | n == 1    = (tail $ map snd $ sortBy (\a b -> compare b a) $ map swap $ Map.toList m, taxicab (Coord 0 0) c)
    | otherwise = spiral (n - 1) (Map.insert c n m) d' (nextCoord c d')
    where
        d_turn = turn LEFT d
        c_turn = nextCoord c d_turn
        d'     = if Map.member c_turn m then d else d_turn
    

findSum :: Int -> Map Coord Int -> [Coord] -> Int
findSum i m (c@(Coord x y):cs)
    | s > i     = s
    | otherwise = findSum i (Map.insert c s m) cs
    where
        s = sum $ map (\cc -> Map.findWithDefault 0 cc m) [(Coord a b) | a <- [x-1..x+1], b <- [y-1..y+1]]
