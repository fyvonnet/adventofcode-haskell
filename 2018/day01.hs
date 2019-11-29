import            AOC.Common


main :: IO ()
main = do
    raw <- readFile "inputs/day01"
    let nums = map (read . filter (/= '+')) $ lines raw
    print $ sum nums
    print $ snd $ findRepeat $ scanl (+) 0 (cycle nums)
