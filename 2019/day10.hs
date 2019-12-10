import           AOC.Common
import           AOC.Coord
import           Data.List (sortBy)
import           Data.Map (Map, (!))
import qualified Data.Map as M

main :: IO ()
main = do
    --raw <- readFile "inputs/day10-test"
    raw <- readFile "inputs/day10"
    let asteroids = map fst $ filter (\x -> snd x == '#') $ getTextMap raw

    let (count, coord, m) = maximum $ map (func asteroids) asteroids
    print count

    let m' = M.map (sortBy (\a b -> compare (distance coord a) (distance coord b))) m
    print $ (\(Coord x y) -> x * 100 + y) $ (!! 199) $ map (\k -> head (m ! k)) $ M.keys m


    
func lst c = (\x -> (M.size x, c, x)) $ foldl (insertCoord c) M.empty $ filter (/= c) lst

insertCoord :: Coord -> Map Float [Coord] -> Coord -> Map Float [Coord]
insertCoord o m c = M.insert a (c : (M.findWithDefault [] a m)) m
    where a = angle o c

angle :: Coord -> Coord -> Float
angle (Coord p0x p0y) (Coord p1x p1y) = negate $ atan2 (fromIntegral (p1x - p0x)) (fromIntegral (p1y - p0y))

distance (Coord p0x p0y) (Coord p1x p1y) = max (abs (p0x - p1x)) (abs (p0y - p1y))
