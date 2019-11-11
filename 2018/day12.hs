import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Set as S


type Parser = Parsec Void String
type PlantSet = S.Set Int
type RuleSet  = S.Set [Bool]



main :: IO ()
main = do
    (initstate, ruleSet) <- readFile "inputs/day12" >>= parseInput
    let generations = iterate (generation ruleSet) initstate

    print $ sum $ S.toList (generations !! 20)

    let (q, r) = (\n -> quotRem n 1000) $ sum $ S.toList (generations !! 1000)
    print (q * 50000000000 `div` 1000 + r)



generation :: RuleSet -> PlantSet -> PlantSet
generation rs ps = foldl (updatePot rs ps) S.empty [S.findMin ps - 2 .. S.findMax ps + 2]



updatePot :: RuleSet -> PlantSet -> PlantSet -> Int -> PlantSet
updatePot rs is ps n = if S.member pots rs then S.insert n ps else ps
    where pots = map (flip S.member is) [n-2..n+2]



parseInput :: String -> IO (PlantSet, RuleSet)
parseInput raw = do
    case parse inputFile "" raw of
        Left e  -> error $ errorBundlePretty e
        Right x -> return x



inputFile :: Parser (PlantSet, RuleSet)
inputFile = do
    initstate <- string "initial state: " *> many ruleChar <* eol
    eol
    rules <- many inputRule
    let mkPlantSet = S.fromList . map fst . filter (\(_, c) -> c == '#') . zip [0..]
    return (mkPlantSet initstate, S.fromList $ catMaybes rules)



inputRule :: Parser (Maybe [Bool])
inputRule = do
    comb <- many ruleChar
    string " => "
    res <- ruleChar
    eol

    if res == '.' then return Nothing
    else return $ Just $ map (== '#') comb



ruleChar :: Parser Char
ruleChar = oneOf "#."
