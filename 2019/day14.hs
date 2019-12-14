
import           Data.List (foldl')
import           Data.Map (Map, (!))
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, parse, many, some, sepBy, errorBundlePretty)
import           Text.Megaparsec.Char (letterChar, numberChar, string, char, newline)
import qualified Data.Map as M

type Parser = Parsec Void String
type Graph  = Map String (Int, [(String, Int)])
data MyState = MyState
    { _graph :: Graph
    , _queue :: [(String, Int)]
    , _nbOre :: Int
    , _stock :: Map String Int
    } deriving (Show)



main :: IO ()
main = do
    input <- readFile "inputs/day14" >>= parseInput
    let graph = foldl' makeGraph M.empty input
    print $ oreCount graph 1
    print $ binSearch graph 0 2000000 


binSearch :: Graph -> Int -> Int -> Int
binSearch graph a b
    | a + 1 == b = a
    | otherwise = if (oreCount graph mid) > 1000000000000 then binSearch graph a mid else binSearch graph mid b
    where mid = a + ((b - a) `div` 2)


oreCount :: Graph -> Int -> Int
oreCount graph n = _nbOre $ until (null . _queue) run (MyState graph [("FUEL", n)] 0 M.empty)


run :: MyState -> MyState
run (MyState graph queue nbOre stock)
    | needed == 0   = (MyState graph (tail queue)  nbOre           stock )
    | name == "ORE" = (MyState graph (tail queue) (nbOre + needed) stock )
    | otherwise     = (MyState graph       queue'  nbOre           stock')  where
        (name, needed) = head queue
        instock = M.findWithDefault 0 name stock
        (stock', toMake) = if instock > needed then (M.insert name (instock - needed + extra) stock, 0) else (M.insert name extra stock, needed - instock)
        (qtprod, prereq) = graph ! name
        nbatch = ceiling ((fromIntegral toMake) / (fromIntegral qtprod))
        extra  = nbatch * qtprod - toMake
        queue' = (tail queue) ++ map (\(n, q) -> (n, nbatch * q)) prereq


makeGraph :: Graph -> ([(String, Int)], (String, Int)) -> Graph
makeGraph m (deps, (name, val)) = M.insert name (val, deps) m


parseInput :: String -> IO [([(String, Int)], (String, Int))]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x


inputLine :: Parser ([(String, Int)], (String, Int))
inputLine = do
    reqs <- concat <$> (some chemical) `sepBy` (string ", ")
    string " => "
    rep <- chemical
    newline
    return (reqs, rep)


chemical :: Parser (String, Int)
chemical = do
    n <- read <$> some numberChar
    char ' '
    str <- some letterChar
    return (str, n)

