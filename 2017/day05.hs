import           Control.Lens ((^.), (%~), (&))
import           Data.List.PointedList (PointedList, focus)
import           Data.Maybe (fromJust)
import qualified Data.List.PointedList as PL


main :: IO ()
main = do
    input <- fromJust <$> PL.fromList <$> map read <$> lines <$> readFile "inputs/day05"

    print $ jump (+1) 1 input
    print $ jump (\o -> if o >= 3 then o - 1 else o + 1) 1 input


jump :: (Int -> Int) -> Int -> PointedList Int -> Int
jump f n pl = case PL.moveN (pl^.focus) (pl & focus %~ f) of
    Nothing  -> n
    Just pl' -> jump f (n + 1) pl'
