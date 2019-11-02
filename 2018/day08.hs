{-# Language DeriveFoldable #-}

import           Control.Monad (replicateM)
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import qualified Data.Map as M

data Node a = Node [Node a] [a] deriving Foldable
type Parser = Parsec Void String


main :: IO ()
main = do
    root <- readFile "inputs/day08" >>= parseNode
    print $ sum root
    print $ nodeValue root



nodeValue :: Node Int -> Int
nodeValue (Node []       metadata) = sum metadata
nodeValue (Node children metadata) = sum $ map nodeValue refChildren
    where
        childrenMap = M.fromList $ zip [1..] children
        refChildren = map fromJust $ filter isJust $ map (\k -> M.lookup k childrenMap) metadata
        
    

parseNode :: String -> IO (Node Int)
parseNode raw = do
    case parse node "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x



node :: Parser (Node Int)
node = do
    nc <- number
    nm <- number
    c  <- replicateM nc node
    m  <- replicateM nm number
    return (Node c m)
    


number :: Parser Int
number = do
    n <- read <$> many (oneOf ['0'..'9'])
    oneOf [' ', '\n']
    return n
