import qualified  Data.Set as S


main :: IO ()
main = do
    raw <- readFile "inputs/day01"
    let nums = map (read . filter (/= '+')) $ lines raw
    print $ sum nums
    print $ findRepeat S.empty (cycle nums) 0


findRepeat :: S.Set Int -> [Int] -> Int -> Int
findRepeat s (n:ns) a
    | S.member a s = a
    | otherwise = findRepeat (S.insert a s) ns (n + a)
