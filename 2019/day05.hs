import           Data.List.Split (splitOn)
import           Data.Vector (Vector, (!), (//))
import           IntCode
import qualified Data.Vector as V

main :: IO ()
main = do
    input <- V.fromList <$> map read <$> splitOn "," <$> readFile "inputs/day05"
    print $ map (last . snd . curry runIntCode input) [1,5]
