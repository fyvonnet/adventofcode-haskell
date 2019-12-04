import Data.List
import Data.List.Split



main :: IO ()
main = do
    input <- map (\s -> read s :: Int) <$> splitOn "-" <$> readFile "inputs/day04"
    print $ foldl' valid (0,0) $ filter ascendant $ map show [input !! 0 .. input !! 1]



valid :: (Int, Int) -> [Char] -> (Int, Int)
valid (c1, c2) lst = (c1', c2') where
    len2s = filter (>= 2) $ map length $ group lst
    c1'   = if (not $ null len2s) then (c1 + 1) else c1
    c2'   = if (elem 2 len2s)     then (c2 + 1) else c2



ascendant :: [Char] -> Bool
ascendant [_] = True
ascendant lst 
    | a > b     = False
    | otherwise = ascendant (tail lst)
    where (a:b:_) = lst
