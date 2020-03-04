{-# LANGUAGE TemplateHaskell #-}

import           AOC.Common
import           AOC.Coord
import           Control.Lens (_1, _2, makeLenses, over, set, use)
import           Control.Lens.Operators ((.=), (%=), (+=), (<>=))
import           Control.Monad (when)
import           Control.Monad.Loops (iterateUntil)
import           Control.Monad.State (execState, State)
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
    let (scaffold, (c, d)) = foldl (flip makeData) (S.empty, undefined) $ getTextMap $ map chr output
    let (RS _ _ ms _ _ s)  = execState (iterateUntil (== True) moveRobot) (RS d c mempty 0 scaffold 0)

    print s
    print ms

    prog <- map ord <$> readFile "day17-prog"
    let (output2, _) = runIntCode prog $ writeMemory 0 2 ics
    print $ last output2


moveRobot :: State RobotState Bool
moveRobot = do
    c@(Coord x y)  <- use coord
    ad <- use direc
    s  <- use scaff

    case map (\rd -> S.member (relNeighbour ad rd c) s) [LEFT, FRONT, RIGHT] of

        [l, True, r] -> do
            ad <- use direc
            when (l && r && (ad `elem` [NORTH, SOUTH])) (sumap += (x * y))
            steps += 1
            coord %= relNeighbour ad FRONT
            return False
        
        [l, False, r] -> do
            st <- use steps
            when (st > 0) (mvmts <>= [FORWARD st])
            case (l, r) of
                (False, False) -> return True
                (True,  False) -> turnRobot LEFT
                (False,  True) -> turnRobot RIGHT


turnRobot :: RelDirection -> State RobotState Bool
turnRobot rd = do
    steps .= 0
    mvmts <>= [TURN rd]
    direc  %= turn rd
    return False


makeData :: (Coord, Char) -> Data -> Data
makeData (c, '#') = over _1 (S.insert c)
makeData (c, '^') = set  _2 (c, NORTH)
makeData (c, '>') = set  _2 (c, EAST )
makeData (c, 'v') = set  _2 (c, SOUTH)
makeData (c, '<') = set  _2 (c, WEST )
makeData  _       = id
