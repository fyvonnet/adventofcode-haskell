import           Data.List.Split (splitOn)
import           Data.Vector (Vector, (!), (//))
import           IntCode
import qualified Data.Vector as V



main :: IO ()
main = do
    input <- V.fromList <$> map (\x -> read x :: Int) <$> splitOn "," <$> readFile "inputs/day05"
    print $ map (\i -> last $ snd $ runIntCode (input, i)) [1,5]



