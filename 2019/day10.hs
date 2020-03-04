import           AOC.Common
import           AOC.Coord
import           Data.List (sortBy, maximumBy)
import           Data.Map (Map, (!))
import qualified Data.Map as M


main :: IO ()
main = do
    raw <- readFile "inputs/day10"
    let asteroids = map fst $ filter (\x -> snd x == '#') $ getTextMap raw

    let (coord, astByAngle) = maximumBy compareLength $ map (func asteroids) asteroids
    print $ M.size astByAngle

    let astByAngle' = map (sortBy (compareDistance coord)) $ M.elems astByAngle
    print $ (\(Coord x y) -> x * 100 + y) $ (destroyAsteroids astByAngle') !! 199


destroyAsteroids :: [[Coord]] -> [Coord]
destroyAsteroids lst = snd $ until (null . fst) laserRotation (lst, [])


laserRotation :: ([[Coord]], [Coord]) -> ([[Coord]], [Coord])
laserRotation (lst, dest) = (lst', dest') where
    dest' = dest ++ map head lst
    lst'  = filter (not . null) $ map tail lst


func :: [Coord] -> Coord -> (Coord, Map Float [Coord])
func lst c = (c, foldl (insertCoord c) M.empty $ filter (/= c) lst)


insertCoord :: Coord -> Map Float [Coord] -> Coord -> Map Float [Coord]
insertCoord o m c = M.insert a (c : (M.findWithDefault [] a m)) m
    where a = angle o c


angle :: Coord -> Coord -> Float
angle (Coord p0x p0y) (Coord p1x p1y) = negate $ atan2 (fromIntegral (p1x - p0x)) (fromIntegral (p1y - p0y))


compareLength :: (Coord, Map Float [Coord]) -> (Coord, Map Float [Coord]) -> Ordering
compareLength (_, a) (_, b) = compare (M.size a) (M.size b)


compareDistance :: Coord -> Coord -> Coord -> Ordering
compareDistance orig a b = compare (distance orig a) (distance orig b)
    

distance :: Coord -> Coord -> Int
distance (Coord p0x p0y) (Coord p1x p1y) = max (abs (p0x - p1x)) (abs (p0y - p1y))
