import qualified Data.Vector      as V
import qualified Data.List.Zipper as Z
import           Control.Monad ((>>=))
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char


type Circle = Z.Zipper Int
type Scores = V.Vector Int
type GameState = (Circle, Scores)



main :: IO ()
main = do
    (nPlayers, lastMarble) <- readFile "inputs/day09" >>= parseInput
    print $ run nPlayers  lastMarble
    print $ run nPlayers (lastMarble * 100)



run :: Int -> Int -> Int
run n l = V.maximum $ snd $ foldl gameTurn ((Z.fromList [0]), (V.replicate n 0)) [1..l]



gameTurn :: GameState -> Int -> GameState
gameTurn (circle, scores) m = do
    if m `mod` 23 == 0 then do
        let np       = V.length scores
        let p        = mod (m - 1) np
        let circle'  = moveLeft 8 circle
        let points   = (scores V.! p) + m + (Z.cursor circle')
        let scores'  = scores V.// [(p, points)]
        let circle'' = moveRight $ Z.delete circle'
        (circle'', scores')
    else do
        let circle'  = moveRight $ Z.insert m $ Z.right circle
        (circle', scores)



moveRight :: Circle -> Circle
moveRight circle
    | Z.endp circle' = Z.start circle'
    | otherwise      = circle'
    where circle' = Z.right circle


    
moveLeft :: Int -> Circle -> Circle
moveLeft 0 circle = circle
moveLeft n circle 
    | Z.beginp circle = moveLeft (n - 1) $ Z.left (Z.end circle)
    | otherwise       = moveLeft (n - 1) (Z.left circle)



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
