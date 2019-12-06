import           Data.List.Split (splitOn)
import           Data.List (foldl')
import           Data.Map (Map, (!))
import           Data.Set (Set)
import           Data.Tuple (swap)
import qualified Data.Map as M
import qualified Data.Set as S

type Queue = [(String, Int)]
type Graph = Map String [String]



main :: IO ()
main = do
    input <- map ((\(a:b:[]) -> (a, b)) . splitOn ")") <$> lines <$> readFile "inputs/day06"
    let makeGraph = foldl' (\m (k, v) -> M.insertWith (++) k [v] m)
    let orbMap  = makeGraph M.empty input
    print $ countOrbits [("COM", 0)] orbMap 0
    
    let orbMap2 = makeGraph orbMap (map swap input)
    let dest = head (orbMap2 ! "YOU")
    let orig = head (orbMap2 ! "SAN")
    print $ findPath [(orig, 0)] orbMap2 dest S.empty



findPath :: Queue -> Graph -> String -> Set String -> Int
findPath ((n,d):xs) m dest v 
    | n == dest = d
    | otherwise = findPath queue m dest v' where
        neighbs = [x | x <- m ! n, not $ S.member x v]
        queue = xs ++ (zip neighbs (repeat (d + 1)))
        v' = foldl' (\s e -> S.insert e s) v neighbs



countOrbits :: Queue -> Graph -> Int -> Int
countOrbits []         _ acc = acc
countOrbits ((n,d):xs) m acc = countOrbits queue m (acc + d) where
    queue = case M.lookup n m of
        Nothing -> xs
        Just os -> xs ++ (zip os (repeat (d + 1)))
