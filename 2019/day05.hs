{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens (makeLenses, over, use, view, (%=), (+=))
import           Control.Monad.Loops (iterateWhile)
import           Control.Monad.State (execState, State)
import           Data.List.Split (splitOn)
import           Data.Vector (Vector, (!), (//))
import           Text.Printf (printf)
import qualified Data.Vector as V

import Debug.Trace


data Param = Value Int | Addr Int deriving (Show)
data Command = ADD | MULTIPLY | INPUT | OUTPUT | EXIT deriving (Show)
data Instr = Instr Command Param Param Param deriving (Show)
data ICState = ICState { _intCode :: Vector Int, _pointer :: Int, _input :: Int, _output :: [Int] } deriving Show

makeLenses ''ICState



main :: IO ()
main = do
    input <- V.fromList <$> map (\x -> read x :: Int) <$> splitOn "," <$> readFile "inputs/day05"
    let r = execState (iterateWhile (== True) (decode >>= exec)) (ICState input 0 1 [])
    print $ last $ _output r



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
            "99" -> EXIT
            otherwise -> error ("Wrong opcode: " ++ oc) 
    let ps = map makeInstr $ zip [m1,m2,m3] $ map (\x -> ic ! (p + x)) [1..3]
    return (Instr (command oc) (ps !! 0) (ps !! 1) (ps !! 2))



exec :: Instr -> (State ICState) Bool

exec i | trace (show i) False = undefined

exec (Instr ADD a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    intCode %= changeIC addr (va + vb)
    pointer += 4
    return True

exec (Instr MULTIPLY a b (Addr addr)) = do
    va <- getVal a
    vb <- getVal b
    intCode %= changeIC addr (va * vb)
    pointer += 4
    return True

exec (Instr INPUT (Addr addr) _ _) = do
    pointer += 2
    vi <- use input
    intCode %= changeIC addr vi
    return True

exec (Instr OUTPUT a _ _) = do
    va <- getVal a
    pointer += 2
    output %= (\l -> l ++ [va])
    return True

exec (Instr EXIT _ _ _) = do
    return False

exec i = do
    error ("Wrong instruction: " ++ show i)



changeIC :: Int -> Int -> Vector Int -> Vector Int
changeIC addr val v = v // [(addr, val)]



getVal :: Param -> (State ICState) Int
getVal p = do
    ic <- use intCode
    case p of
        Value v -> return v
        Addr  a -> return $ (ic ! a)
