import           Data.IntMap (IntMap, (!))
import           IntCode
import qualified Data.IntMap as IM


main :: IO ()
main = do
    ics <- loadCode "inputs/day23"

    let computers = [snd $ runIntCode [addr] ics | addr <- [0..49]]
    let network   = replicate 50 []
    print $ runAllComputers network computers


runAllComputers :: [[Int]] -> [ICState] -> Int
runAllComputers net comps =
    case processOutputs $ map fst results of
        Left [_,y] -> y
        Right net' -> runAllComputers net' $ map snd results
    where results  = zipWith runOneComputer net comps
    

runOneComputer :: [Int] -> ICState -> ([Int], ICState)
runOneComputer []  ics = runIntCode [-1] ics
runOneComputer net ics = runIntCode net  ics


processOutputs :: [[Int]] -> Either [Int] [[Int]]
processOutputs outputs
    | IM.member 255 net = Left  (net ! 255)
    | otherwise         = Right (IM.elems net) where
        net = go (IM.fromList [(addr, []) | addr <- [0..49]]) $ concat outputs
        go :: IntMap [Int] -> [Int] -> IntMap [Int]
        go net []              = net
        go net (addr:x:y:rest) = go (IM.insertWith (\a b -> b ++ a) addr [x, y] net) rest
