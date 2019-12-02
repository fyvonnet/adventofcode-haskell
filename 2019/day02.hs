import           Data.List.Split (splitOn)
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V



main :: IO ()
main = do
    raw <- readFile "inputs/day02"
    let input = V.fromList $ map read (splitOn "," raw)
    let start v v1 v2 = run (v // [(1, v1), (2, v2)]) 0
    print $ start input 12 2
    let (n, v) = head $ [(n, v) | n <- [0..99], v <- [0..99], start input n v == 19690720]
    print (100 * n + v)



run :: Vector Int -> Int -> Int
run v i
    | opcode ==  1 = exec (+)
    | opcode ==  2 = exec (*)
    | opcode == 99 = v ! 0
    | otherwise    = error ("wrong opcode: " ++ (show opcode))
    where
        opcode = v ! i
        exec f = run (v // [(v ! (i + 3), f (v ! (v ! (i + 1))) (v ! (v ! (i + 2))))]) (i + 4)

