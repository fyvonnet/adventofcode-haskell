{-# LANGUAGE TemplateHaskell #-}

--module IntCode (runIntCode, Return) where
module IntCode where


import           Control.Lens (makeLenses, over, use, view, set, (%=), (+=), (.=))
import           Control.Monad.Loops (iterateWhile)
import           Control.Monad.State (execState, State, when, unless)
import           Data.List (foldl')
import           Data.List.Split (splitOn)
-- import           Data.Vector (Vector, (!), (//))
import           Data.IntMap (IntMap, (!))
import           Text.Printf (printf)
-- import qualified Data.Vector as V
import qualified Data.IntMap    as M

import Debug.Trace


data Param = Value Int | Addr Int deriving (Show)
data Command = ADD | MULTIPLY | INPUT | OUTPUT | JMPIFTRUE | JMPIFFALSE | LTHAN | EQUALS | CHRELBASE | EXIT deriving (Show)
data Instr = Instr Command Param Param Param deriving (Show)
data ICState = ICState
    { _intCode :: IntMap Int
    , _pointer :: Int
    , _relbase :: Int
    , _input   :: [Int]
    , _output  :: [Int]
    , _running :: Bool
    } deriving Show

makeLenses ''ICState



loadCode :: FilePath -> IO ICState
loadCode fp = do
    intcode <- map read <$> splitOn "," <$> readFile fp
    return (ICState (M.fromList $ zip [0..] intcode) 0 0 [] [] True)


runIntCode :: ICState -> ICState
runIntCode ics = execState (iterateWhile (== True) (decode >>= exec)) ics

setInput :: [Int] -> ICState -> ICState
setInput = set input

appendInput :: [Int] -> ICState -> ICState
appendInput i = over input (++ i)

getOutput :: ICState -> [Int]
getOutput = reverse . view output

isRunning :: ICState -> Bool
isRunning = view running

resetOutput :: ICState -> ICState
resetOutput = set output []

changeCode :: [(Int, Int)] -> ICState -> ICState
changeCode changes = over intCode (\m -> foldl' (\m (k, v) -> M.insert k v m) m changes)

readMemory :: Int -> ICState -> Int
readMemory i = flip (!) i . view intCode


decode :: (State ICState) Instr
decode = do
    p  <- use pointer
    ic <- use intCode
    rb <- use relbase
    let (m3:m2:m1:oc) = (printf "%05d" (ic ! p)) :: String

    let makeInstr m v = do
        case m of
            '0' -> Addr  v
            '1' -> Value v
            '2' -> Addr (v + rb)
            otherwise -> error ("Wrong mode: " ++ [m])

    let command str = do
        case str of
            "01" -> ADD
            "02" -> MULTIPLY
            "03" -> INPUT
            "04" -> OUTPUT
            "05" -> JMPIFTRUE
            "06" -> JMPIFFALSE
            "07" -> LTHAN
            "08" -> EQUALS
            "09" -> CHRELBASE
            "99" -> EXIT
            otherwise -> error ("Wrong opcode: " ++ oc) 
    let ps = zipWith makeInstr [m1,m2,m3] $ map (\x -> ic ! (p + x)) [1..3]
    return (Instr (command oc) (ps !! 0) (ps !! 1) (ps !! 2))



exec :: Instr -> (State ICState) Bool

-- exec i | trace (show i) False = undefined

exec (Instr ADD a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    changeIC addr (va + vb)
    pointer += 4
    return True

exec (Instr MULTIPLY a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    changeIC addr (va * vb)
    pointer += 4
    return True

exec (Instr INPUT (Addr addr) _ _) = do
    i <- use input
    case i of
        []     -> return False
        (vi:_) -> do
            pointer += 2
            changeIC addr vi
            input %= tail
            return True

exec (Instr OUTPUT a _ _) = do
    va <- getVal a
    pointer += 2
    output %= (\l -> va : l)
    return True

exec (Instr JMPIFTRUE a b _) = do
    va <- getVal a
    vb <- getVal b
    if (va /= 0) then pointer .= vb else pointer += 3
    return True
    
exec (Instr JMPIFFALSE a b _) = do
    va <- getVal a
    vb <- getVal b
    if (va == 0) then pointer .= vb else pointer += 3
    return True

exec (Instr LTHAN a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    changeIC addr (if va < vb then 1 else 0)
    pointer += 4
    return True

exec (Instr EQUALS a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    changeIC addr (if va == vb then 1 else 0)
    pointer += 4
    return True

exec (Instr CHRELBASE a _ _) = do
    va <- getVal a
    relbase += va
    pointer += 2
    return True

exec (Instr EXIT _ _ _) = do
    running .= False
    return False

exec i = error ("Wrong instruction: " ++ show i)



changeIC :: Int -> Int -> (State ICState) ()
changeIC addr val =
    intCode %= (\m -> M.insert addr val m)



getVal :: Param -> (State ICState) Int
getVal p = do
    ic <- use intCode
    case p of
        Value v -> return v
        Addr  a -> return $ (ic ! a)
