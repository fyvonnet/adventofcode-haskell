main :: IO ()
main = do
    input <- map read <$> lines <$> readFile "inputs/day01"
    let fuelReq = (\m -> (m `div` 3) - 2)
    let allFuelReqs = map fuelReq input

    print $ sum allFuelReqs
    print $ sum $ map (sum . takeWhile (> 0) . iterate fuelReq) allFuelReqs
