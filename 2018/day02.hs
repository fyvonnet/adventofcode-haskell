
import           Data.List
import           Data.Maybe



main :: IO ()
main = do
    input <- lines <$> readFile "inputs/day02"
    print $ uncurry (*) $ foldl twoThree (0, 0) input
    putStrLn $ compareAllIDs input (tail input)



-- check if a string contains two and three times the same character
twoThree :: (Int, Int) -> String -> (Int, Int)
twoThree (two, three) id = (two', three')
    where
        charCounts = map length $ group $ sort id
        two'       = if elem 2 charCounts then two   + 1 else two
        three'     = if elem 3 charCounts then three + 1 else three



-- compare all IDs of a list with each other
compareAllIDs :: [String] -> [String] -> String
compareAllIDs (_  :id1s)  [] = compareAllIDs id1s (tail id1s)
compareAllIDs (id1:id1s) (id2:id2s)
    | isNothing result = compareAllIDs (id1:id1s) id2s
    | otherwise        = fromJust result
    where
        result = compareIDs id1 id2 0 ""



-- compare two IDs
-- return common characters if only one character is different
compareIDs :: String -> String -> Int -> String -> Maybe String
compareIDs  []       []      0     _   = Nothing
compareIDs  []       []      1     str = Just $ reverse str
compareIDs (c1:c1s) (c2:c2s) count str
    | count > 1 = Nothing
    | c1 == c2  = compareIDs c1s c2s  count      (c1 : str)
    | otherwise = compareIDs c1s c2s (count + 1)       str

