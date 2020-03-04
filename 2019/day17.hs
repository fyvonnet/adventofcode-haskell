{-# LANGUAGE TemplateHaskell #-}

import           AOC.Common
import           AOC.Coord
import           Control.Lens ((&), _1, _2, makeLenses, over, set, view)
import           Data.Char (chr, ord) 
import           Data.Foldable (foldl')
import           Data.Set (Set)
import           IntCode
import qualified Data.Set as S


type Robot    = (Coord, AbsDirection)
type Scaffold = Set Coord
type Data     = (Scaffold, Robot)

data Movement = TURN RelDirection | FORWARD Int

instance Show Movement where
    show (TURN LEFT ) = "L"
    show (TURN RIGHT) = "R"
    show (FORWARD n ) = show n


data RobotState = RS
    { _direc :: AbsDirection
    , _coord :: Coord
    , _mvmts :: [Movement]
    , _steps :: Int
    , _scaff :: Scaffold
    , _sumap :: Int
    }
   
makeLenses ''RobotState


main :: IO ()
main = do
    ics <- loadCode "inputs/day17"

    let (output, _)        = runIntCode [] ics
    let (scaffold, (c, d)) = foldl' (flip makeData) (S.empty, undefined) $ getTextMap $ map chr output
    let (s, ms)            = moveRobot (RS d c mempty 0 scaffold 0)

    print s
    print ms

    prog <- map ord <$> readFile "day17-prog"
    let (output2, _) = runIntCode prog $ writeMemory 0 2 ics
    print $ last output2


moveRobot :: RobotState -> (Int, [Movement])
moveRobot rs@(RS ad c@(Coord x y) _ st s _) =
    case map (\rd -> S.member (relNeighbour ad rd c) s) [LEFT, FRONT, RIGHT] of
        [False, False, False] -> (view sumap rs, reverse $ view mvmts rs')
        [True,  False, False] -> moveRobot $ turnRobot LEFT  rs'
        [False, False, True ] -> moveRobot $ turnRobot RIGHT rs'
        [l,     True,  r    ] -> moveRobot $ rs
            & over sumap (if (l && r && (ad `elem` [NORTH, SOUTH])) then (+ (x * y)) else id)
            & over steps (+ 1)
            & over coord (relNeighbour ad FRONT)
    where rs' = if st > 0 then over mvmts ((:) (FORWARD st)) rs else rs


turnRobot :: RelDirection -> RobotState -> RobotState
turnRobot rd rs = rs
    & set steps 0
    & over mvmts ((:) (TURN rd))
    & over direc (turn rd)


makeData :: (Coord, Char) -> Data -> Data
makeData (c, '#') = over _1 (S.insert c)
makeData (c, '^') = set  _2 (c, NORTH)
makeData (c, '>') = set  _2 (c, EAST )
makeData (c, 'v') = set  _2 (c, SOUTH)
makeData (c, '<') = set  _2 (c, WEST )
makeData  _       = id
