
import           AOC.Coord
import           Data.Maybe
import qualified Data.Map as Map


data Direction = WEST | NORTH | EAST | SOUTH deriving Enum
data Turn      = LEFT | STRAIGHT | RIGHT deriving Enum

type CartState  = (Turn, Direction)
type TrackMap   = Map.Map Coord (CartState -> CartState)
type CartMap    = Map.Map Coord CartState
type ParseState = (TrackMap, CartMap, Coord)
data RunState = RS { _cm :: CartMap, _fc :: Maybe Coord, _lc :: Maybe Coord }



main :: IO ()
main = do
    (tm, cm, _) <- foldl makeMaps (Map.empty, Map.empty, (Coord 0 0)) <$> readFile "inputs/day13"
    let result   = until (isJust . _lc) (\rs -> foldl (moveCart tm) rs (Map.keys $ _cm rs)) (RS cm Nothing Nothing)
    let printAns = (putStrLn . showCoord . fromJust)
    printAns $ _fc result
    printAns $ _lc result



moveCart :: TrackMap -> RunState -> Coord -> RunState
moveCart tm (RS cm fc _) c
    | isNothing state   = (RS cm  fc  Nothing)
    | Map.null cm'      = (RS cm' fc  (Just c'))
    | Map.member c' cm' = (RS cmd fc' Nothing)
    | otherwise         = (RS cmi fc  Nothing)
    where
        state  = Map.lookup c cm
        cm'    = Map.delete c cm
        c'     = move c $ fromJust state
        state' = Map.findWithDefault id c' tm $ fromJust state
        fc'    = if isNothing fc then Just c' else fc
        cmd    = Map.delete c' cm'
        cmi    = Map.insert c' state' cm'



makeMaps :: ParseState -> Char -> ParseState
makeMaps (tm, cm, coord) c
    | c == '<'  = insertcm WEST
    | c == '^'  = insertcm NORTH
    | c == '>'  = insertcm EAST
    | c == 'v'  = insertcm SOUTH
    | c == '+'  = inserttm intersec
    | c == '/'  = inserttm turn1
    | c == '\\' = inserttm turn2
    | c == '\n' = (tm, cm, (Coord 0 (_y coord + 1)))
    | otherwise = (tm, cm, nextChar)
    where
        nextChar   = (Coord (_x coord + 1) (_y coord))
        insertcm d = (tm, (Map.insert coord (LEFT, d) cm), nextChar)
        inserttm f = ((Map.insert coord f tm), cm, nextChar)



move :: Coord -> CartState -> Coord
move (Coord x y) (_, WEST ) = (Coord (x-1)  y   )
move (Coord x y) (_, NORTH) = (Coord  x    (y-1))
move (Coord x y) (_, EAST ) = (Coord (x+1)  y   )
move (Coord x y) (_, SOUTH) = (Coord  x    (y+1))

-- +
intersec :: CartState -> CartState
intersec (LEFT,     d) = (STRAIGHT, turnLeft  d)
intersec (STRAIGHT, d) = (RIGHT,              d)
intersec (RIGHT,    d) = (LEFT,     turnRight d)

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

turnRight :: Direction -> Direction
turnRight SOUTH = WEST
turnRight d     = succ d

turnLeft :: Direction -> Direction
turnLeft WEST = SOUTH
turnLeft d    = pred d

