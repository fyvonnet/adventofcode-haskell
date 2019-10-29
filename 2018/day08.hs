import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Control.Monad.State


data Node = Node [Node] [Int] deriving Show



main :: IO ()
main = do
    raw <- readFile "inputs/day08"
    let root = evalState makeNode $ map read $ words raw
    print $ sumMetadata root
    print $ nodeValue   root



sumMetadata :: Node -> Int
sumMetadata (Node children metadata) = (sum metadata) + (sum $ map sumMetadata children)



nodeValue :: Node -> Int
nodeValue (Node []       metadata) = sum metadata
nodeValue (Node children metadata) =
    let childrenMap = Map.fromList $ zip [1..] children
        refChildren = map fromJust $ filter isJust $ map (\k -> Map.lookup k childrenMap) metadata
    in sum $ map nodeValue refChildren
        
    

makeNode :: State [Int] Node
makeNode = do
    lst <- get
    let (nc:nm:rem) = lst
    put rem
    children <- reverse <$> foldM addChild [] [1..nc]
    lst <- get
    let (metadata,rem2) = splitAt nm lst
    put rem2
    return (Node children metadata)



addChild :: [Node] -> Int -> State [Int] [Node]
addChild lst _ = do
    node <- makeNode
    return (node:lst)


