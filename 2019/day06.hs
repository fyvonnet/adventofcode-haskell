import           Data.List.Split (splitOn)
import           Data.List (foldl')
import           Data.Map (Map, (!))
import           Data.Set (Set)
import           Data.Tuple (swap)
import qualified Data.Map as M
import qualified Data.Set as S



main :: IO()
main = do
    input <- map ((\(a:b:[]) -> (a, b)) . splitOn ")") <$> lines <$> readFile "inputs/day06"
    let makeMap = foldl' (\m (k, v) -> M.insertWith (++) k [v] m) M.empty
    let orbMap  = makeMap input
    print $ countOrbits [("COM", 0)] orbMap 0
    
    let orbMap2 = M.unionWith (++) orbMap (makeMap $ map swap input)
    let dest = head (orbMap2 ! "YOU")
    let orig = head (orbMap2 ! "SAN")
    print $ findPath orbMap2 [(orig, 0)] dest S.empty


findPath :: Map String [String] -> [(String, Int)] -> String -> Set String -> Int
findPath m ((n,d):xs) dest v 
    | n == dest = d
    | otherwise = findPath m queue dest v' where
        neighbs = [x | x <- m ! n, not $ S.member x v]
        queue = xs ++ (zip neighbs (repeat (d + 1)))
        v' = foldl' (\s e -> S.insert e s) v neighbs


countOrbits :: [(String, Int)] -> Map String [String] -> Int -> Int
countOrbits []         _ acc = acc
countOrbits ((n,c):xs) m acc = countOrbits queue m (acc + c) where
    queue = case M.lookup n m of
        Nothing -> xs
        Just os -> xs ++ (zip os (repeat (c + 1)))

    
