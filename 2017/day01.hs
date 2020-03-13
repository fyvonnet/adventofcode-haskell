main :: IO ()
main = do
    input <-  map (\x -> read [x]) <$> init <$> readFile "inputs/day01"
    print $ computeSum 1 input
    print $ computeSum ((length input) `div` 2)  input


computeSum :: Int -> [Int] -> Int
computeSum n lst = foldl go 0 $ zip lst $ drop n $ cycle lst
    where go s (a, b) = if a == b then s + a else s
