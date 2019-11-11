import           Data.List.PointedList.Circular
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Vector      as V


type Circle = PointedList Int
type Scores = V.Vector Int
type GameState = (Circle, Scores, Int)



main :: IO ()
main = do
    (nPlayers, lastMarble) <- readFile "inputs/day09" >>= parseInput
    let iterations = iterate (gameTurn nPlayers) ((singleton 0), (V.replicate nPlayers 0), 1)
    let printMaxScore = print . V.maximum . (\(_,s,_) -> s) . (!!) iterations
    printMaxScore lastMarble
    printMaxScore (lastMarble * 100)



gameTurn :: Int -> GameState -> GameState
gameTurn np (circle, scores, m) = (circle', scores', (m + 1))
    where
        special = m `mod` 23 == 0
        player  = mod (m - 1) np
        (rm, spcirc) = specialTurn circle
        points  = (scores V.! player) + m + rm
        scores' = if special then scores V.// [(player, points)] else scores
        circle' = if special then spcirc else insertRight m $ next circle



specialTurn :: Circle -> (Int, Circle)
specialTurn circle = (rm, fromJust $ deleteRight circle')
    where
        circle' = (iterate previous circle) !! 7
        rm      = _focus circle'



parseInput :: String -> IO (Int, Int)
parseInput raw = do
    case parse inputLine "" raw of 
        Left  e -> error $ show e
        Right x -> return x



inputLine :: Parsec () String (Int, Int)
inputLine = do
    nPlayers   <- read <$> many numberChar
    string " players; last marble is worth "
    lastMarble <- read <$> many numberChar
    string " points\n"
    return (nPlayers, lastMarble)
