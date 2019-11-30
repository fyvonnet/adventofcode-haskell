{-# LANGUAGE QuasiQuotes                #-}

import           Text.RE.TDFA.String
import           Text.RE.Replace
import qualified Data.Map as M



type Coord    = (Int, Int)
type CountMap = M.Map Coord Int
data Claim    = Claim
    { num :: Int        -- claim number
    , coords :: [Coord] -- coordinates of all claimed squares
    }



main :: IO ()
main = do
    claims <- parseInput <$> readFile "inputs/day03"
    let countMap = foldl (M.unionWith (+)) M.empty (map (\c -> M.fromList [(crd, 1) | crd <- coords c]) claims)
    print $ length $ filter (>1) $ M.elems countMap
    print $ findNoOverlaps claims countMap



findNoOverlaps :: [Claim] -> CountMap -> Int
findNoOverlaps []     _ = error "Can't find claim without overlap"
findNoOverlaps (c:cs) m 
    | and (map ((== 1) . ((M.!) m)) $ coords c) = num c
    | otherwise = findNoOverlaps cs m



parseInput :: String -> [Claim]
parseInput raw = map (parseLine r) input where
    input = lines raw
    r = either error id $ compileRegex "^#(@{%int}) @ (@{%int}),(@{%int}): (@{%int})x(@{%int})$"



parseLine :: RE -> String -> Claim
parseLine re s = (Claim num [(x, y) | x <- [cx..(cx + w - 1)], y <- [cy..(cy + h - 1)]]) where
    ms  = s ?=~ re
    num = read $ ms !$$ [cp|1|]
    cx  = read $ ms !$$ [cp|2|]
    cy  = read $ ms !$$ [cp|3|]
    w   = read $ ms !$$ [cp|4|]
    h   = read $ ms !$$ [cp|5|]
