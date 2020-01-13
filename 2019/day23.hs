import           Control.Monad.State (evalState, get, put)
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
        Left y     -> y
        Right net' -> runAllComputers net' $ map snd results
    where results  = zipWith runOneComputer net comps
    

runOneComputer :: [Int] -> ICState -> ([Int], ICState)
runOneComputer []  ics = runIntCode [-1] ics
runOneComputer net ics = runIntCode net  ics


processOutputs :: [[Int]] -> Either Int [[Int]]
processOutputs outputs = go (IM.fromList [(addr, []) | addr <- [0..49]]) $ concat outputs where
    go :: IntMap [Int] -> [Int] -> Either Int [[Int]]
    go net []              = Right $ IM.elems net
    go _   (255 :_:y:_)    = Left y
    --go net (addr:x:y:rest) = go (IM.insertWith (\a b -> b ++ a) addr [x, y] net) rest
    go net (addr:x:y:rest) = go (IM.insertWith (flip (++)) addr [x, y] net) rest
