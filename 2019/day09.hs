import IntCode

main :: IO ()
main = do
    input <- loadCode "inputs/day09"
    print $ getOutput $ runIntCode $ setInput [1] input
    print $ getOutput $ runIntCode $ setInput [2] input
