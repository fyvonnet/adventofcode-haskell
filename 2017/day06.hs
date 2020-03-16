import           AOC.Common (findRepeat)
import           Control.Lens ((^.), (+~), (&), _1, _2, set)
import           Data.Foldable (toList)
import           Data.List.PointedList.Circular (focus)
import           Data.Maybe (fromJust)
import qualified Data.List.PointedList.Circular as PL

type IntPair = (Int, Int)


main :: IO ()
main = do
    input <- map read <$> words <$> readFile "inputs/day06"

    let ((f, s), _) = findRepeat $ iterate redistribute input
    print  s
    print (s - f)


redistribute :: [Int] -> [Int]
redistribute lst = go mv (set focus 0 $ PL.moveN mi $ fromJust $ PL.fromList lst) 
    where 
        (mi, mv) = foldl1 firstMax $ zip [0..] lst
        go 0 cl  = toList cl
        go n cl  = go (n - 1) ((PL.next cl) & focus +~ 1)
   

firstMax :: IntPair -> IntPair -> IntPair
firstMax a b = if a^._2 >= b^._2 then a else b
