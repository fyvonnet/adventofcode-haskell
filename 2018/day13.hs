
import           AOC.Coord
import           AOC.Common (getTextMap)
import           Data.Maybe
import qualified Data.Map as Map


type CartState  = (RelDirection, AbsDirection)
type TrackMap   = Map.Map Coord (CartState -> CartState)
type CartMap    = Map.Map Coord CartState
type ParseState = (TrackMap, CartMap)
data RunState   = RS { _cm :: CartMap, _fc :: Maybe Coord, _lc :: Maybe Coord }



main :: IO ()
main = do
    (tm, cm) <- foldl makeMaps (Map.empty, Map.empty) <$> getTextMap <$> readFile "inputs/day13"
    let result   = until (isJust . _lc) (\rs -> foldl (moveCart tm) rs (Map.keys $ _cm rs)) (RS cm Nothing Nothing)
    let printAns = putStrLn . showCoord . fromJust
    printAns $ _fc result
    printAns $ _lc result



moveCart :: TrackMap -> RunState -> Coord -> RunState
moveCart tm (RS cm fc _) c
    | isNothing state   = (RS cm  fc  Nothing)   -- trying to move a previously-deleted cart
    | Map.null cm'      = (RS cm' fc  (Just c')) -- only one cart left, returning its coordinates after the last move
    | Map.member c' cm' = (RS cmd fc' Nothing)   -- collision between two carts, removing the second cart
    | otherwise         = (RS cmi fc  Nothing)   -- cart moving: insert back in the map with new coords and updated state
    where
        state  = Map.lookup c cm
        cm'    = Map.delete c cm
        c'     = absNeighbour (snd $ fromJust state) c
        state' = Map.findWithDefault id c' tm $ fromJust state
        fc'    = if isNothing fc then Just c' else fc
        cmd    = Map.delete c' cm'
        cmi    = Map.insert c' state' cm'



makeMaps :: ParseState -> (Coord, Char) -> ParseState
makeMaps (tm, cm) (coord, c)
    | c == '<'  = insertcm WEST
    | c == '^'  = insertcm NORTH
    | c == '>'  = insertcm EAST
    | c == 'v'  = insertcm SOUTH
    | c == '+'  = inserttm intersec
    | c == '/'  = inserttm turn1
    | c == '\\' = inserttm turn2
    | otherwise = (tm, cm)
    where
        insertcm d = (tm, (Map.insert coord (LEFT, d) cm))
        inserttm f = ((Map.insert coord f tm), cm)



-- +
intersec :: CartState -> CartState
intersec (LEFT,  d) = (FRONT, turn LEFT  d)
intersec (FRONT, d) = (RIGHT,            d)
intersec (RIGHT, d) = (LEFT,  turn RIGHT d)

-- /
turn1 :: CartState -> CartState
turn1 (t, WEST)  = (t, SOUTH)
turn1 (t, NORTH) = (t, EAST)
turn1 (t, EAST)  = (t, NORTH)
turn1 (t, SOUTH) = (t, WEST)

-- \
turn2 :: CartState -> CartState
turn2 (t, WEST)  = (t, NORTH)
turn2 (t, NORTH) = (t, WEST)
turn2 (t, EAST)  = (t, SOUTH)
turn2 (t, SOUTH) = (t, EAST)
