import Data.List (sort, tails)

main :: IO ()
main = do
    input <- map (reverse . sort . map read . words) <$> lines <$> readFile "inputs/day02"
    print $ sum $ map (uncurry (-)) [(head lst, last lst) | lst <- input]
    print $ sum $ map (uncurry div) $ filter divisible $ concat $ map pairs input
    where
        pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]
        divisible (a, b) = mod a b == 0
