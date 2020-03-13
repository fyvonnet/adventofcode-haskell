import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do
    input <- map words <$> lines <$> readFile "inputs/day04"
    print $ countValid input
    print $ countValid $ map (map sort) input

countValid :: [[String]] -> Int
countValid  = length . filter (== True) . map isValid

isValid :: [String] -> Bool
isValid lst = and $ map (== 1) $ Map.elems $ foldl go Map.empty lst
    where go s w = Map.insertWith (+) w 1 s
