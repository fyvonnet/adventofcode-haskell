import           AOC.Coord
import           IntCode



main :: IO ()
main = do
    ics <- loadCode "inputs/day13"

    let (output1, _) = runIntCode [] ics
    print $ countBlocks output1

    let (_, _, output2) = until (\(_, ics, _) -> not $ isRunning ics) play (0, writeMemory 0 2 ics, [])
    print $ searchScore output2


play :: (Int, ICState, [Int]) -> (Int, ICState, [Int])
play (joy, ics, _) = (joy', ics', output) where
    (output, ics') = runIntCode [joy] ics
    joy' = joySetting output


countBlocks :: [Int] -> Int
countBlocks output = go output 0 where
    go []          c = c
    go (_:_:id:xs) c
        | id == 2   = go xs (c + 1)
        | otherwise = go xs  c


joySetting :: [Int] -> Int
joySetting output = 
    case (uncurry compare $ searchPB output) of
        LT ->  1
        EQ ->  0
        GT -> -1


searchScore :: [Int] -> Int
searchScore (-1:0:score:_) = score
searchScore (x:y:_:rs)     = searchScore rs


searchPB :: [Int] -> (Int, Int)
searchPB output = go (Nothing, Nothing) output where
    go (Just p, Just b) _ = (p, b)
    go  _        [] = (0, 0)
    go (mp, mb) (x:_:id:rs)
        | id == 3   = go (Just x, mb) rs
        | id == 4   = go (mp, Just x) rs
        | otherwise = go (mp, mb) rs
