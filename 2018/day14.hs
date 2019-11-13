import           Data.List
import           Data.String.Utils (rstrip)
import qualified Data.Sequence as S



main :: IO ()
main = do
    input <- rstrip <$> readFile "inputs/day14"
    let scores = 3:7:(recipes (S.fromList [3, 7]) 0 1)
    putStrLn $ concatMap show $ take 10 $ drop (read input) scores
    print $ countRecipes (map (\x -> read [x]) input) scores 0



recipes :: S.Seq Int -> Int -> Int -> [Int]
recipes sb elf1 elf2 = nd ++ recipes sb' elf1' elf2'
    where
        rec1   = S.index sb elf1
        rec2   = S.index sb elf2
        (x, y) = (rec1 + rec2) `divMod` 10
        nd     = if x == 0 then [y] else [x, y]
        sb'    = sb S.>< S.fromList nd
        elf1'  = (elf1 + rec1 + 1) `mod` S.length sb'
        elf2'  = (elf2 + rec2 + 1) `mod` S.length sb'



countRecipes :: [Int] -> [Int] -> Int -> Int
countRecipes p lst c
    | isPrefixOf p lst = c
    | otherwise        = countRecipes p (tail lst) (c + 1)
