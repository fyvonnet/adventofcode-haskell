import           AOC.Coord
import           Data.List.Split (splitOn)
import qualified Data.Map as M



main :: IO ()
main = do
    input <- map (splitOn ",") <$> lines <$> readFile "inputs/day03"
    let [circ1, circ2] = map (M.fromList . flip zip [1..] . foldl traceLine []) input
    let intersections  = map (\(c, s) -> (taxicab (Coord 0 0) c, s)) $ M.toList $ M.intersectionWith (+) circ1 circ2
    print $ map (\f -> minimum $ map f intersections) [fst, snd]



traceLine :: [Coord] -> String -> [Coord]
traceLine cs str = (++) cs $ lineCoords c d n where
    c = if null cs then (Coord 0 0) else last cs
    d = head str
    n = read $ tail str



lineCoords :: Coord -> Char -> Int -> [Coord]
lineCoords (Coord x y) 'U' n =           [(Coord x ys) | ys <- [(y + 1) .. (y + n)]]
lineCoords (Coord x y) 'D' n = reverse $ [(Coord x ys) | ys <- [(y - n) .. (y - 1)]]
lineCoords (Coord x y) 'R' n =           [(Coord xs y) | xs <- [(x + 1) .. (x + n)]]
lineCoords (Coord x y) 'L' n = reverse $ [(Coord xs y) | xs <- [(x - n) .. (x - 1)]]
