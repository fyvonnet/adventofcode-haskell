{-# LANGUAGE TemplateHaskell #-}

import           AOC.Common
import           AOC.Coord
import           Control.Lens ((.~), (<>~), (%~), (+~), (&), _1, _2, makeLenses)
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
    { _mvmts :: [Movement]
    , _sumap :: Int
    , _direc :: AbsDirection
    , _coord :: Coord
    , _steps :: Int
    , _scaff :: Scaffold
    }
   
makeLenses ''RobotState


main :: IO ()
main = do
    ics <- loadCode "inputs/day17"

    let (output, _)        = runIntCode [] ics
    let (scaffold, (c, d)) = foldl' (flip makeData) (S.empty, undefined) $ getTextMap $ map chr output
    let (RS ms s _ _ _ _)  = moveRobot (RS [] 0 d c 0 scaffold)

    print $ div s 2
    print $ tail ms
    

    prog <- map ord <$> readFile "day17-prog"
    let (output2, _) = runIntCode prog $ writeMemory 0 2 ics
    print $ last output2


moveRobot :: RobotState -> RobotState
moveRobot rs@(RS _ sm ad c@(Coord x y) st sc) =
    case map (\rd -> S.member (relNeighbour ad rd c) sc) [LEFT, FRONT, RIGHT] of
        [False, False, False] -> rs'
        [True,  False, False] -> moveRobot $ turnRobot LEFT  rs'
        [False, False, True ] -> moveRobot $ turnRobot RIGHT rs'
        [l,     True,  r    ] -> moveRobot $ rs
            & sumap +~ (if (l && r) then (x * y) else 0)
            & steps +~ 1
            & coord %~ relNeighbour ad FRONT
    where rs' = rs & mvmts <>~ [FORWARD st]


turnRobot :: RelDirection -> RobotState -> RobotState
turnRobot rd rs = rs
    & steps  .~ 0
    & direc  %~  turn rd
    & mvmts <>~ [TURN rd]


makeData :: (Coord, Char) -> Data -> Data
makeData (c, '#') = _1 %~ S.insert c
makeData (c, '^') = _2 .~ (c, NORTH)
makeData (c, '>') = _2 .~ (c, EAST )
makeData (c, 'v') = _2 .~ (c, SOUTH)
makeData (c, '<') = _2 .~ (c, WEST )
makeData  _       = id
