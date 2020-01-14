import Data.Char (chr, ord)
import IntCode

main :: IO ()
main = do
    ics <- loadCode "inputs/day25"
    --putStr $ fst $ runIntCodeASCII "east" ics
    run "" ics

run :: String -> ICState -> IO ()
run str ics = do
    let (output, ics') = runIntCodeASCII str ics
    putStr output
    if isRunning ics'
    then do
        str' <- getLine
        run str' ics'
    else return ()

