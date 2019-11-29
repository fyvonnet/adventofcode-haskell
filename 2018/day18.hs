import           Data.List  (sort, group)
import           Data.Map   (Map, (!))
import           Data.Maybe (catMaybes)
import qualified Data.Map as M 

data Acre = OPEN | TREES | LUMBER deriving (Show, Eq, Ord)
type Landscape = Map (Int, Int) Acre



main :: IO ()
main = do
    raw <- readFile "inputs/day18"
    let input   = M.fromList [((x, y), getAcre c) | (y, cs) <- zip [0..] (lines raw), (x, c) <- zip [0..] cs]
    let changes = iterate change input
    let val m   = (a * b) where [a, b] = map length $ group $ sort $ filter (/= OPEN) $ M.elems (changes !! m)
    let (a, b)  = findRepeat changes
    print $ val 10
    print $ val $ (a + mod (1000000000 - b) (b - a))



findRepeat :: Ord a => [a] -> (Int, Int)
findRepeat lst = go (zip lst [0..]) M.empty where
    go ((k, v) : xs) m =
        case M.lookup k m of
          Nothing -> go xs (M.insert k v m)
          Just x  -> (x, v)
    


change :: Landscape -> Landscape
change l = M.fromList $ map go (M.keys l) where
    go c = (c, a') where
        (x, y)  = c
        sc      = [((x + x'), (y + y')) | x' <- [-1..1], y' <- [-1..1], (x', y') /= (0, 0)]
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
