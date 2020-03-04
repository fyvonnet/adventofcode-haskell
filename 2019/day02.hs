import           IntCode


main :: IO ()
main = do
    input <- loadCode "inputs/day02"
    let start v1 v2 = readMemory 0 $ snd $ runIntCode [] $ foldl (\i (a, v) -> writeMemory a v i) input [(1, v1), (2, v2)]
    print $ start 12 2
    print $ head $ [100 * n + v | n <- [0..99], v <- [0..99], start n v == 19690720]
