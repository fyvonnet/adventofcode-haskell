import           AOC.Common
import           AOC.Coord
import           Data.Ix (range)
import           Data.List (foldl', sort)
import           Data.Set (Set)
import qualified Data.Set as S

type Bugs = Set Coord


main :: IO ()
main = do
    input <- getTextMap <$> readFile "inputs/day24"
    let bugs = S.fromList $ map fst $ filter ((=='#') . snd)input
    let (_, bugs') = findRepeat $ iterate minute bugs
    print $ totalRating bugs'


totalRating :: Bugs -> Int
totalRating bugs = sum $ map (squareRating bugs) allCoords


squareRating :: Bugs -> Coord -> Int
squareRating bugs coord@(Coord x y)
    | isBug bugs coord = 2 ^ (y * 5 + x)
    | otherwise        = 0
    

minute :: Bugs -> Bugs
minute bugs = foldl' (nextState bugs) bugs allCoords


nextState :: Bugs -> Bugs -> Coord -> Bugs
nextState init bugs coord = do
    let nbugs = countBugs init coord
    if isBug init coord
    then
        if nbugs == 1
        then bugs
        else S.delete coord bugs
    else
        if nbugs == 1 || nbugs == 2
        then S.insert coord bugs
        else bugs


isBug :: Bugs -> Coord -> Bool
isBug = flip S.member 


countBugs :: Bugs -> Coord -> Int
countBugs bugs = length . filter (== True) . adjacents bugs


adjacents :: Bugs -> Coord -> [Bool]
adjacents bugs coord = map (isBug bugs) (allNextCoords coord)


allCoords :: [Coord]
allCoords = range ((Coord 0 0), (Coord 4 4))
