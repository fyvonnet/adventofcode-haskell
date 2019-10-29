
import           Control.Monad.Reader
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Tuple
import           Data.Void
import           Data.List
import           Data.Char
import           Text.Megaparsec
import           Text.Megaparsec.Char



type Parser   = Parsec Void String
type RulesMap = Map.Map Char [Char]
type WorkState = (RulesMap, RulesMap, Int)

data Task   = Task
    { time :: Int   -- remaining time
    , name :: Char  -- task name
    } deriving (Eq, Ord)



main :: IO ()
main = do
    input <- readFile "inputs/day07" >>= parseInput
    let emptyMap  = Map.fromList [(l, []) | l <- ['A'..'Z']]
    let makeMap l = Map.map sort $ foldl (\m (k, v) -> Map.adjust (v:) k m) emptyMap l
    let nxtMap    = makeMap input
    let prqMap    = makeMap $ map swap input
    let start     = map fst $ filter (null . snd) $ Map.toList prqMap 
    let run n     = runReader (multiWorkers [] start [] 0) (nxtMap, prqMap, n)
    putStrLn $ fst $ run 1
    print    $ snd $ run 5



multiWorkers :: [Task] -> [Char] -> [Char] -> Int -> Reader WorkState ([Char], Int)
multiWorkers []   []    done tm = return (reverse done, tm)
multiWorkers wrks queue done tm = do
    (nxtMap, prqMap, _) <- ask
    (tasks, queue')     <- distributeTasks wrks queue
    let (ft:ts) = tasks
    let done'   = (name ft) : done
    let tm'     = tm + (time ft)
    let wrks'   = map (\(Task t n) -> (Task (t - (time ft)) n)) ts
    let queue'' = sort (queue' ++ [n | n <- neighbors (name ft) nxtMap, and $ map (\x -> any (== x) done') $ neighbors n prqMap])
    multiWorkers wrks' queue'' done' tm'
    where neighbors k m = Map.findWithDefault [] k m



distributeTasks :: [Task] -> [Char] -> Reader WorkState ([Task], [Char])
distributeTasks wrks queue = do
    (_, _, nwrks) <- ask
    if (length wrks == nwrks) || (null queue) 
        then return (sort wrks, queue)
        else distributeTasks ((newTask $ head queue):wrks) (tail queue)



newTask :: Char -> Task
newTask c = (Task ((ord c) - (ord 'A') + 61) c)
    


parseInput :: String -> IO [(Char, Char)]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left  e -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser (Char, Char)
inputLine = do
    string "Step "
    a <- upperChar
    string " must be finished before step "
    b <- upperChar
    string " can begin."
    eol
    return (a, b)
