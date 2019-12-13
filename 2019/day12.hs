{-# LANGUAGE QuasiQuotes #-}

import           AOC.Common
import           Control.Applicative (liftA2)
import           Data.FixedList (FixedList3, Cons((:.)), Nil)
import           Data.Foldable (toList)
import           Data.List (delete)
import           Text.RE.TDFA.String (RE, compileRegex, cp, (?=~))
import           Text.RE.Replace ((!$$))
import qualified Data.FixedList as F


type MoonCoord = (FixedList3 Int)
type Moon = (MoonCoord, MoonCoord)

main :: IO ()
main = do
    --input <- lines <$> readFile "inputs/day12-test"
    input <- lines <$> readFile "inputs/day12"
    let re = either error id $ compileRegex "^<x=(@{%int}), y=(@{%int}), z=(@{%int})>$"
    let positions  = map (parseLine re) input
    let velocities = replicate (length positions) (F.fromFoldable' $ repeat 0)

    let moons = zipWith (\a b -> (a, b)) positions velocities
    let allUpdates = iterate updateMoons' moons
    let finalUpdate = allUpdates !! 1000

    print $ sum $ map totalEnergy finalUpdate

    --print $ fmap (!!0) $ map fst finalUpdate

    --let fu = allUpdates !! 0
    --print fu
    let ((_, a), _) = findRepeat $ map (map (\(p, v) -> (toList p !! 0, toList v !! 0))) allUpdates
    let ((_, b), _) = findRepeat $ map (map (\(p, v) -> (toList p !! 1, toList v !! 1))) allUpdates
    let ((_, c), _) = findRepeat $ map (map (\(p, v) -> (toList p !! 2, toList v !! 2))) allUpdates
    print ((a `div` 2) * (b `div` 2) * (c `div` 2))
    --print $ map (map (\(p, v) -> (toList p !! 0, toList v !! 0))) allUpdates


totalEnergy :: (MoonCoord, MoonCoord) -> Int
totalEnergy (pos, vel) = (energy pos) * (energy vel)

energy :: MoonCoord -> Int
energy = sum . fmap abs

--totalEnergy :: ([MoonCoord], [MoonCoord]) -> Int
--totalEnergy (poss, vels) = ke + pe where
--    ke = sum $ map (sum . abs) toList poss
--    pe = sum $ map (sum . abs) toList vels
--
updateMoons' :: [(MoonCoord, MoonCoord)] -> [(MoonCoord, MoonCoord)]
updateMoons' moons = map (updateOneMoon moons) moons

updateOneMoon :: [Moon] -> Moon -> Moon
updateOneMoon allMoons (pos, vel) = (pos', vel') where
    allPos = filter (/= pos) $ map fst allMoons
    vel' = vel + (F.fromFoldable' $ applyGravity (map toList allPos) (toList pos))
    pos' = pos + vel'

updateMoons :: ([MoonCoord], [MoonCoord]) -> ([MoonCoord], [MoonCoord])
updateMoons (poss, vels) = (poss', vels') where
    vels' = zipWith (+) vels (getVelocities poss)
    poss' = zipWith (+) poss vels'

--updateOneMoon :: [Moon] -> Moon -> Moon
--updateOneMoon all (pos, vel) = (pos', vel') where
--    others = delete moon all


getVelocities :: [MoonCoord] -> [MoonCoord]
getVelocities mcs = map F.fromFoldable' vels where
    lsts = map toList mcs
    vels = map (\e -> applyGravity (delete e lsts) e) lsts

-- compute velocity of a moon
applyGravity :: [[Int]] -> [Int] -> [Int]
applyGravity _    []  = []
applyGravity lsts ref = v : applyGravity (map tail lsts) (tail ref) where
    v = computeVel (map head lsts) (head ref)

-- compute velocity in one dimension
computeVel :: [Int] -> Int -> Int
computeVel lst r = sum $ map (go r) lst where
    go r c = case compare r c of
        LT ->  1
        EQ ->  0
        GT -> -1
        



parseLine :: RE -> String -> MoonCoord
parseLine re str = x :. y  :. z :. F.Nil where
    matches = str ?=~ re
    x = read $ matches !$$ [cp|1|]
    y = read $ matches !$$ [cp|2|]
    z = read $ matches !$$ [cp|3|]
