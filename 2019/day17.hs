import           AOC.Common
import           AOC.Coord
import           Data.Char (chr, ord) 
import           Data.Set (Set)
import           IntCode
import qualified Data.Set as S

main :: IO ()
main = do
    ics <- loadCode "inputs/day17"

    let (output, _) = runIntCode [] ics
    let scaffold = S.fromList $ map fst $ filter ((== '#') . snd) $ getTextMap $ map chr output
    print $ sum $ map (\(Coord x y) -> x * y) $ filter (intersection scaffold) $ S.toList scaffold

    prog <- map ord <$> readFile "day17-prog"
    let (output2, _) = runIntCode prog $ writeMemory 0 2 ics
    print $ last output2

intersection :: (Set Coord) -> Coord -> Bool
intersection s c = and $ map (flip S.member s . nextCoord c) allAbsDirections
