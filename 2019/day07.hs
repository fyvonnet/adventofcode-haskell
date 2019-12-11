import           IntCode
import           Data.List (foldl', permutations)


import Debug.Trace


main :: IO ()
main = do
    ics <- loadCode "inputs/day07"
    print $ map (getMaxPower ics) [(0, 4), (5, 9)]
    

getMaxPower :: ICState -> (Int, Int) -> Int
getMaxPower ics (a, b) = maximum $ map go $ permutations [a..b] where
    go s = runAllAmps (map (\x -> [x]) s) [0] (replicate 5 ics)


runAllAmps :: [[Int]] -> [Int] -> [ICState] -> Int
runAllAmps s input amps 
    | isRunning lastICS = runAllAmps [[],[],[],[],[]] output amps'
    | otherwise         = head output where
    (amps', output) = foldl' runOneAmp ([], input) (zip amps s)
    lastICS = last amps'
            
        
runOneAmp :: ([ICState], [Int]) -> (ICState, [Int]) -> ([ICState], [Int])
runOneAmp (icss, input) (ics, s) = (icss ++ [ics'], output) where
    (output, ics') = runIntCode (s ++ input) ics
