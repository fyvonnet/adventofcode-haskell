
import qualified Data.Map    as M
import qualified Data.Vector as V
import           Data.List
import           Data.Time
import           Data.Tuple
import           Data.Maybe
import           Text.Regex.Posix



type Pair       = (Int, Int)
type GuardData  = [(Int, [Pair])] -- ID and list of minute data (minute number and time slept)
type Event      = (UniversalTime, Maybe Int)
type MinuteVec  = V.Vector Int
type GuardMap   = M.Map Int MinuteVec



main :: IO ()
main = do
    input <- (sort . map parseLine . lines) <$> readFile "inputs/day04"
    let guardData = createGuardData input 0 V.empty M.empty
    let run strat = print $ uncurry (*) $ snd $ foldl (applyStrat strat) (0, (0, 0)) guardData
    run (\ms -> (sum $ map fst ms, snd $ maximum ms))
    run maximum



applyStrat :: ([Pair] -> Pair) -> (Int, Pair) -> (Int, [Pair]) -> (Int, Pair)
applyStrat f current (gid, minutes) = max current (localMax, (gid, maxminute))
    where (localMax , maxminute) = f minutes



createGuardData :: [Event] -> Int -> MinuteVec -> GuardMap -> GuardData

createGuardData [] id v m =
    M.toList $ M.map (\v -> zip (V.toList v) [0..]) $ M.insert id v m

createGuardData ((_, Just id'):es) id v m = do
    let m' = if V.null v then m else M.insert id v m
    let v' = M.findWithDefault (V.replicate 60 0) id' m'
    createGuardData es id' v' m'

createGuardData ((fa, Nothing):(wu, Nothing):es) id v m = do 
    let minute = read . formatTime defaultTimeLocale "%M"
    let v'     = V.accum (+) v [(m, 1) | m <- [(minute fa)..((minute wu) - 1)]]
    createGuardData es id v' m



parseLine :: String -> Event
parseLine s =
    let dateTime = fromJust (parseTimeM True defaultTimeLocale "[%Y-%m-%d %H:%M]" (s =~ "\\[(.*)\\]"))
        maybeGID = (read . tail) <$> (s =~~ "#[0-9]+")
    in (dateTime, maybeGID)

