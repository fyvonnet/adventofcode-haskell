import           IntCode
import           Data.List.Split (splitOn)
import           Data.Vector ((//), (!))
import qualified Data.Vector as V



main :: IO ()
main = do
    input <- V.fromList <$> map read <$> splitOn "," <$> readFile "inputs/day02"
    let start v1 v2 = (fst $ runIntCode ((input // [(1, v1), (2, v2)]), undefined)) ! 0
    print $ start 12 2
    print $ head $ [100 * n + v | n <- [0..99], v <- [0..99], start n v == 19690720]
