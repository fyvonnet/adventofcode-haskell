{-# LANGUAGE TemplateHaskell #-}

module IntCode (runIntCode) where


import           Control.Lens (makeLenses, over, use, view, (%=), (+=), (.=))
import           Control.Monad.Loops (iterateWhile)
import           Control.Monad.State (execState, State, when, unless)
import           Data.Vector (Vector, (!), (//))
import           Text.Printf (printf)
import qualified Data.Vector as V

import Debug.Trace


data Param = Value Int | Addr Int deriving (Show)
data Command = ADD | MULTIPLY | INPUT | OUTPUT | JMPIFTRUE | JMPIFFALSE | LTHAN | EQUALS | EXIT deriving (Show)
data Instr = Instr Command Param Param Param deriving (Show)
data ICState = ICState { _intCode :: Vector Int, _pointer :: Int, _input :: Int, _output :: [Int] } deriving Show

makeLenses ''ICState



runIntCode :: (Vector Int, Int) -> (Vector Int, [Int])
runIntCode (ic, input) = (_intCode r, reverse $ _output r) where
    r = execState (iterateWhile (== True) (decode >>= exec)) (ICState ic 0 input [])



decode :: (State ICState) Instr
decode = do
    p  <- use pointer
    ic <- use intCode
    let (m3:m2:m1:oc) = (printf "%05d" (ic ! p)) :: String

    let makeInstr (m, v) = do
        case m of
            '0' -> Addr v
            '1' -> Value v
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
            "99" -> EXIT
            otherwise -> error ("Wrong opcode: " ++ oc) 
    let ps = map makeInstr $ zip [m1,m2,m3] $ map (\x -> ic ! (p + x)) [1..3]
    return (Instr (command oc) (ps !! 0) (ps !! 1) (ps !! 2))



exec :: Instr -> (State ICState) Bool

--exec i | trace (show i) False = undefined

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
    pointer += 2
    vi <- use input
    changeIC addr vi
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

exec (Instr EXIT _ _ _) = return False

exec i = error ("Wrong instruction: " ++ show i)



changeIC :: Int -> Int -> (State ICState) ()
changeIC addr val =
    intCode %= (\v -> v // [(addr, val)])



getVal :: Param -> (State ICState) Int
getVal p = do
    ic <- use intCode
    case p of
        Value v -> return v
        Addr  a -> return $ (ic ! a)
