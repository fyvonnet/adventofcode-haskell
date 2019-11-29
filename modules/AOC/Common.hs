module AOC.Common where


import           AOC.Coord
import qualified Data.Map as M


type TextMapElem = (Coord, Char)
type TextMap = [TextMapElem]



getTextMap :: String -> TextMap
getTextMap s = [((Coord x y), c) | (y, cs) <- zip [0..] (lines s), (x, c) <- zip [0..] cs]



findRepeat :: Ord a => [a] -> ((Int, Int), a)
findRepeat lst = go (zip lst [0..]) M.empty where
   go ((k, v) : xs) m =
        case M.lookup k m of
           Nothing -> go xs (M.insert k v m)
           Just x  -> ((x, v), k)

