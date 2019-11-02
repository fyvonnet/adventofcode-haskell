{-# Language DeriveFoldable #-}

import           Control.Monad.State
import           Data.Maybe
import qualified Data.Map as M

data Node a = Node [Node a] [a] deriving Foldable



main :: IO ()
main = do
    root <- evalState makeNode <$> map read <$> words <$> readFile "inputs/day08"
    print $ sum root
    print $ nodeValue   root



nodeValue :: Node Int -> Int
nodeValue (Node []       metadata) = sum metadata
nodeValue (Node children metadata) = sum $ map nodeValue refChildren
    where
        childrenMap = M.fromList $ zip [1..] children
        refChildren = map fromJust $ filter isJust $ map (\k -> M.lookup k childrenMap) metadata
        
    

makeNode :: State [Int] (Node Int)
makeNode = do
    lst <- get
    let (nc:nm:rem) = lst
    put rem

    children <- replicateM nc makeNode

    lst <- get
    let (metadata,rem) = splitAt nm lst
    put rem

    return (Node children metadata)
