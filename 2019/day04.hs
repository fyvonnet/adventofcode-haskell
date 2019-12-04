import Data.List
import Data.List.Split



main :: IO ()
main = do
    input <- map read <$> splitOn "-" <$> readFile "inputs/day04"
    print $ foldl' valid (0,0) [input !! 0 .. input !! 1]



valid :: (Int, Int) -> Int -> (Int, Int)
valid (c1, c2) n = (c1', c2') where
    lst   = show n
    asc   = ascendant lst
    len2s = filter (>= 2) $ map length $ group lst
    two   = not $ null len2s
    stwo  = elem 2 len2s
    c1'   = if (asc &&  two) then (c1 + 1) else c1
    c2'   = if (asc && stwo) then (c2 + 1) else c2



ascendant :: [Char] -> Bool
ascendant [_] = True
ascendant lst 
    | a > b     = False
    | otherwise = ascendant (tail lst)
    where (a:b:_) = lst
