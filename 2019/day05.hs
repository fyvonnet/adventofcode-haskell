import           IntCode

main :: IO ()
main = do
    ics <- loadCode "inputs/day05"
    print $ map (\i -> last $ getOutput $ runIntCode $ setInput [i] ics) [1,5]
