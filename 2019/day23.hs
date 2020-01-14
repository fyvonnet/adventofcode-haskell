 {-# LANGUAGE TemplateHaskell #-}

import           Control.Lens        ((.=), makeLenses, use)
import           Control.Monad       (when, zipWithM)
import           Control.Monad.State (evalState, State)
import           Data.IntMap         (IntMap)
import           Data.Maybe          (fromJust, isNothing)
import           IntCode             (ICState, loadCode, runIntCode)
import qualified Data.IntMap as IM


data NatState = NS
    { _content    :: [Int]
    , _lastSent :: [Int]
    , _firstY     :: Maybe Int
    }

makeLenses ''NatState


main :: IO ()
main = do
    ics <- loadCode "inputs/day23"
    print $ evalState (runAllComputers [[addr, -1] | addr <- [0..49]] (repeat ics)) (NS [] [] Nothing)


runAllComputers :: [[Int]] -> [ICState] -> State NatState (Int, Int)
runAllComputers inputs comps = do
    if all null inputs then do
        nat <- use content
        ls  <- use lastSent
        if nat == ls then do
            let [_, y2] = nat
            y1 <- fromJust <$> use firstY
            return (y1, y2)
        else do
            lastSent .= nat
            runAllComputers (nat:(repeat [])) comps
    else do
        (outputs, comps') <- unzip <$> zipWithM runOneComputer inputs comps
        inputs' <- processOutputs outputs
        runAllComputers inputs' comps'
    

runOneComputer :: [Int] -> ICState -> State NatState ([Int], ICState)
runOneComputer input ics = return $ runIntCode (if null input then [-1] else input) ics


processOutputs :: [[Int]] -> State NatState [[Int]]
processOutputs outputs = go (IM.fromList [(addr, []) | addr <- [0..49]]) $ concat outputs where
    go :: IntMap [Int] -> [Int] -> State NatState [[Int]]
    go net []              = return $ IM.elems net
    go net (255:x:y:rest)  = do
        y1 <- use firstY
        when (isNothing y1) (firstY .= Just y)
        content .= [x, y]
        go net rest
    go net (addr:x:y:rest) = go (IM.insertWith (flip (++)) addr [x, y] net) rest

