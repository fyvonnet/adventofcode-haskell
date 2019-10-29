
import           Data.Char
import           Data.String.Utils
import qualified Data.Map as Map



main :: IO ()
main = do
    let letterValues = zip ['A'..'Z'] [1..]
    let letterMap = Map.fromList $ letterValues ++ [(toLower l, negate v) | (l, v) <- letterValues]
    input <- map ((Map.!) letterMap) <$> rstrip <$> readFile "inputs/day05"
    print $ reduce input []
    print $ minimum [reduce (filter ((/=) s . abs) input) [] | s <- [1..26]]



reduce :: [Int] -> [Int] -> Int
reduce [_]      remain = (+1) $ length remain
reduce (x:y:ys) remain
    | x + y == 0 = uncurry reduce $ pushback ys remain
    | otherwise  = reduce (y:ys) (x:remain)



pushback :: [Int] -> [Int] -> ([Int], [Int])
pushback polymer [] = (polymer, [])
pushback polymer (r:rs) = (r:polymer, rs)
