
import           Data.List
import           Data.List.Split 
import           Data.Time
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Map    as M
import qualified Data.Vector as V



type Pair       = (Int, Int)
type GuardData  = [(Int, [Pair])] -- ID and list of minute data (minute number and time slept)
type MinuteVec  = V.Vector Int
type GuardMap   = M.Map Int MinuteVec
type Parser = Parsec Void String
data Event = Wake | Sleep | Shift Int deriving (Show, Ord, Eq)



main :: IO ()
main = do
    input <- sort <$> (readFile "inputs/day04" >>= parseInput)
    let guardData = createGuardData input 0 V.empty M.empty
    let run strat = uncurry (*) $ snd $ foldl (applyStrat strat) (0, (0, 0)) guardData
    print $ run (\ms -> (sum $ map fst ms, snd $ maximum ms))
    print $ run maximum



applyStrat :: ([Pair] -> Pair) -> (Int, Pair) -> (Int, [Pair]) -> (Int, Pair)
applyStrat f current (gid, minutes) = max current (localMax, (gid, maxminute))
    where (localMax , maxminute) = f minutes



createGuardData :: [(LocalTime, Event)] -> Int -> MinuteVec -> GuardMap -> GuardData

createGuardData [] id v m =
    M.toList $ M.map (\v -> zip (V.toList v) [0..]) $ M.insert id v m

createGuardData ((_, Shift id'):es) id v m = createGuardData es id' v' m'
    where
        m' = if V.null v then M.empty else M.insert id v m
        v' = M.findWithDefault (V.replicate 60 0) id' m'

createGuardData ((ts, Sleep):(tw, Wake):es) id v m = createGuardData es id v' m
    where
        getMin = todMin . localTimeOfDay
        v'     = V.accum (+) v [(m, 1) | m <- [(getMin ts)..((getMin tw) - 1)]]

createGuardData _ _ _ _ = error "something wrong with the events list"



parseInput :: String -> IO [(LocalTime, Event)]
parseInput raw = do
    case parse (many inputLine) "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x



inputLine :: Parser (LocalTime, Event)
inputLine = do
    string "["
    strTime <- many (noneOf "]")
    string "] "
    strEvt  <- many printChar
    eol

    let evt = parseEvent strEvt
    let lt  = parseTimeOrError True defaultTimeLocale "%F %R" strTime
    return (lt, evt)



parseEvent :: String -> Event
parseEvent str = 
    case words str of
        ("falls":_)         -> Sleep
        ("wakes":_)         -> Wake
        ("Guard":('#':n):_) -> Shift $ read n
