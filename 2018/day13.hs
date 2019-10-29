
import qualified Data.Map as Map
import           Data.Maybe
import           Coord



data Direction = WEST | NORTH | EAST | SOUTH deriving Enum
data Turn      = LEFT | STRAIGHT | RIGHT deriving Enum

type CartState  = (Turn, Direction)
type TrackMap   = Map.Map Coord (CartState -> CartState)
type CartMap    = Map.Map Coord CartState
type ParseState = (TrackMap, CartMap, Coord)



main :: IO ()
main = do
    (trackMap, cartMap, _) <- foldl makeMaps (Map.empty, Map.empty, (Coord 0 0)) <$> readFile "inputs/day13"
    let (firstcoll, lastcart) = moveCart trackMap cartMap Nothing []
    putStrLn $ showCoord firstcoll
    putStrLn $ showCoord lastcart



moveCart :: TrackMap -> CartMap -> Maybe Coord -> [Coord] -> (Coord, Coord)
moveCart tm cm fstcoll [] = moveCart tm cm     fstcoll  (Map.keys cm)   -- Queue exhausted. Generate new queue from the carts map.
moveCart tm cm fstcoll (c:cs)
    | Map.null cm'      = (fromJust fstcoll, c')                        -- One cart remaining. Returns its coordinate and first collision coordinates.
    | Map.member c' cm'   = moveCart tm cm_del fstcoll' cs'             -- A collision had happend. Remove second cart from the carts map.
    | otherwise           = moveCart tm cm_ins fstcoll  cs              -- Cart is just moving. Add cart back in the carts map with updated coordinates and state.
    where
        state    = cm Map.! c
        cm'      = Map.delete c cm
        cm_del   = Map.delete c' cm'
        cm_ins   = Map.insert c' state' cm'
        c'       = move c state
        state'   = Map.findWithDefault id c' tm state
        fstcoll' = if isNothing fstcoll then Just c' else fstcoll
        cs'      = filter (/= c') cs



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

