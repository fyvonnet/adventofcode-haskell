import           AOC.Common (findRepeat, getTextMap)
import           AOC.Coord 
import           Data.List  (sort, group)
import           Data.Map   (Map, (!))
import           Data.Maybe (catMaybes)
import qualified Data.Map as M 

data Acre = OPEN | TREES | LUMBER deriving (Show, Eq, Ord)
type Landscape = Map Coord Acre



main :: IO ()
main = do
    raw <- readFile "inputs/day18"
    let input   = M.map getAcre $ M.fromList $ getTextMap raw
    let changes = iterate change input
    let val m   = (a * b) where [a, b] = map length $ group $ sort $ filter (/= OPEN) $ M.elems (changes !! m)
    let ((f, l), _) = findRepeat changes
    print $ val 10
    print $ val $ (f + mod (1000000000 - l) (l - f))



change :: Landscape -> Landscape
change l = M.fromList $ map go (M.keys l) where
    go c = (c, a') where
        (Coord x y)  = c
        sc      = [(Coord (x + x') (y + y')) | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0, 0)]
        count a = length $ filter (== a) $ catMaybes $ map (flip M.lookup l) sc
        a       = l ! c
        a'      = case a of
            OPEN   -> if  (count TREES)  >= 3 then TREES  else a
            TREES  -> if  (count LUMBER) >= 3 then LUMBER else a
            LUMBER -> if ((count LUMBER) >= 1) && ((count TREES) >= 1) then LUMBER else OPEN



getAcre :: Char -> Acre
getAcre c =
    case c of
        '.'       -> OPEN
        '|'       -> TREES
        '#'       -> LUMBER
        otherwise -> error ("Wrong character: " ++ [c])
