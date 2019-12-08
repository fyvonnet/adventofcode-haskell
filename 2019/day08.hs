import Data.List (sort, group, transpose, dropWhile)
import Data.String.Utils (strip)


main :: IO ()
main = do
   layers <- slice (25 * 6) <$> strip <$> readFile "inputs/day08"
   print $ (\[_, a, b] -> a * b) $ head $ sort $ map (map length . group . sort) layers
   putStr $ unlines $ slice 25 $ map ((\c -> if c == '1' then '#'  else ' ') . head . dropWhile (== '2')) $ transpose layers


slice :: Int -> [a] -> [[a]]
slice n lst = go lst [] where
    go []  acc = reverse acc
    go lst acc = go (drop n lst) ((take n lst) : acc)
