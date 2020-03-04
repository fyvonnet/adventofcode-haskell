{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}

import           AOC.Common
import           AOC.Coord
import           Control.Lens (_1, _2, _3, over, set)
import           Control.Monad.Reader (ask, runReader, Reader)
import           Data.Char  (isUpper)
import           Data.List  (find, foldl', groupBy, sort)
import           Data.Map   (Map)
import           Data.Maybe (catMaybes)
import           Data.Set   (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type MazeCoord = (Coord, Int)
type Maze      = Map Coord Object
type StartData = (MazeCoord, MazeCoord, [(Coord, Object)])
type Portal    = (String, Coord, Coord)
type PortFunc  = Side -> Coord -> Int -> Maybe MazeCoord
data Side      = INNER | OUTER deriving (Show, Eq)
data Object    = LETTER Char | WALL | PASSAGE | PORTAL Side Coord deriving (Show, Eq)


main :: IO ()
main = do
    textMap <- getTextMap <$> readFile "inputs/day20"

    let [maxx, maxy] = map (\f -> maximum $ map (f . fst) textMap) [_x, _y]
    let (maze, ltCoords) = foldl' makeMazeMap (M.empty, []) textMap
    let triplets = [makeTriplet maze c dir | c <- ltCoords, dir <- allAbsDirections]
    let portals = groupByName $ catMaybes $ map decodePortal' triplets
    let (entry, exit, portals') = foldl' (parsePortalsLst maxx maxy) (undefined, undefined, []) portals
    let maze' = M.union (M.fromList portals') maze

    print $ map (\f -> findExit f maze' S.empty exit [(entry, 0)]) [portFunc1, portFunc2]


groupByName :: [Portal] -> [[Portal]]
groupByName = groupBy (\(a, _, _) (b, _, _) -> a == b) . sort


parsePortalsLst :: Int -> Int -> StartData -> [Portal] -> StartData
parsePortalsLst _    _    sd [("AA", _, c)]                             = set  _1 (c, 0)  sd
parsePortalsLst _    _    sd [("ZZ", _, c)]                             = set  _2 (c, 0)  sd
parsePortalsLst maxx maxy sd [(_, en1@(Coord x y), ex1), (_, en2, ex2)] = over _3 (++ ps) sd where
    ps = [(en1, (PORTAL sd1 ex2)), (en2, (PORTAL sd2 ex1))]
    (sd1, sd2) = if isOuter1 then (OUTER, INNER) else (INNER, OUTER)
    isOuter1 = x == 1 || y == 1 || x == maxx - 1 || y == maxy - 1


makeTriplet :: Maze -> Coord -> AbsDirection -> [(Coord, Object)]
makeTriplet maze coord dir = [(c, getObj maze c) | c <- [nextCoord coord dir, coord, nextCoord coord $ turn BACK dir]]


makeMazeMap (m, ls) (coord, c)
    | c == '.'  = (M.insert coord PASSAGE m, ls)
    | isUpper c = (M.insert coord (LETTER c) m, coord:ls) 
    | otherwise = (m, ls)


findExit :: PortFunc -> Maze -> Set MazeCoord -> MazeCoord -> [(MazeCoord, Int)] -> Int
findExit pf maze visited exit ((coord, steps):xs)
    | coord == exit = steps
    | otherwise     = findExit pf maze visited' exit (xs ++ nextCoords) where
    visited' = S.insert coord visited
    nextCoords = [(c, steps + 1) | c <- filter (not . flip S.member visited) $ availablePaths pf maze coord]


availablePaths :: PortFunc -> Maze -> MazeCoord -> [MazeCoord]
availablePaths pf maze (coord, level) = catMaybes $ map (maybeNext pf maze) [(c, level) | c <- allNextCoords coord]


maybeNext :: PortFunc -> Maze -> MazeCoord -> Maybe MazeCoord
maybeNext pf maze coord@(c, l) =
    case getObj maze c of
        PORTAL s xc -> pf s xc l
        PASSAGE     -> Just coord
        otherwise   -> Nothing


portFunc1 :: PortFunc
portFunc1 _ c _ = Just (c, 0)

portFunc2 :: PortFunc
portFunc2 OUTER _ 0 = Nothing
portFunc2 OUTER c l = Just (c, l - 1)
portFunc2 INNER c l = Just (c, l + 1)


isLetter :: Object -> Bool
isLetter (LETTER _) = True
isLetter _ = False


decodePortal' :: [(Coord, Object)] -> Maybe Portal

decodePortal' [(exitCoord, PASSAGE), (lt1Coord, LETTER lt1), (lt2Coord, LETTER lt2)] =
    Just (name, entryCoord, exitCoord) where
        name = if lt1Coord < lt2Coord then [lt1, lt2] else [lt2, lt1]
        entryCoord = lt1Coord
         
decodePortal' _ = Nothing

    
getObj :: Maze -> Coord -> Object
getObj = flip (M.findWithDefault WALL)
