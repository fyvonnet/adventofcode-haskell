import           IntCode

main :: IO ()
main = do
    ics <- loadCode "inputs/day05"
    print $ map (\i -> last $ fst $ runIntCode [i] ics) [1,5]
