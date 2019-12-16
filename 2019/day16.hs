import           AOC.Common
import           Data.String.Utils (strip)


main :: IO ()
main = do
    raw <- readFile "inputs/day16"
    let input = map (read . singleton) $ strip raw :: [Int]

    let patterns = map createPattern [1..length input]
    printOutput $ iterate100 (phase1 patterns) input

    let offset = read $ take 7 raw
    let input2 = concat $ replicate 10000 $ reverse input
    let input2' = take (length input2 - offset) input2
    printOutput $ reverse $ iterate100 phase2 input2'


phase1 :: [[Int]] -> [Int] -> [Int]
phase1 ps i = map (applyPattern i) ps


phase2 :: [Int] -> [Int]
phase2 = map (flip mod 10) . tail . scanl (+) 0


iterate100 :: (a -> a) -> a -> a
iterate100 f x = (iterate f x) !! 100


createPattern :: Int -> [Int]
createPattern n = tail $ cycle $ concat $ map (replicate n) [0, 1, 0, -1]
    

applyPattern :: [Int] -> [Int] -> Int
applyPattern i p = mod (abs s) 10
    where s = sum $ zipWith (*) i p


printOutput :: [Int] -> IO ()
printOutput o = putStrLn $ concat $ map show $ take 8 o

