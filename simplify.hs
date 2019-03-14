import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as Set



main :: IO ()
main = do
	arguments <- getArgs
	inputStr <- getInput (getInputFileName arguments) -- getContents
	-- print inputStr
	let cf_grammar = parseInput (lines inputStr)
	-- print  (makeTwoTuples (cfg_rules cf_grammar))
	-- print (makeTwoTuples (Set.toList (cfg_rules cf_grammar)))
	-- putStrLn (getInputFileName arguments)
	printGrammar (getGStripe cf_grammar)
	-- print (getNtSet (Set.toList (cfg_nonterminals cf_grammar)) (Set.toList (cfg_terminals cf_grammar)) (makeTwoTuples (cfg_rules cf_grammar)) (Set.fromList []))
	-- print cf_grammar

-- gets the input file name, if the was no input file name entered, returns empty string
getInputFileName :: [String] -> String
getInputFileName [] = ""
getInputFileName (x:xs)
                   | not (elem x ["-i", "-1", "-2"]) = x
                   | otherwise = getInputFileName xs

type Nonterminal = Char
type Terminal = Char
type StartSymbol = Char

data CFGrammar = CFGrammar {
    cfg_nonterminals::Set.Set Nonterminal,
    cfg_terminals::Set.Set Terminal,
    cfg_startSymbol::StartSymbol,
    cfg_rules::[(Char,String)]
    -- cfg_rules::Set.Set (Nonterminal,String)
} deriving (Show)


parseInput :: [[Char]] -> CFGrammar 
parseInput (nonterminals:terminals:startsymbol:rules) = CFGrammar {
	cfg_nonterminals = parsedNonterminals,
	cfg_terminals = parsedTerminals,
	cfg_startSymbol = startsymbolChecked,
	cfg_rules = parsedRules
} where parsedNonterminals = parseNonterminals nonterminals
        parsedTerminals = parseTerminals terminals
        startsymbolChecked = if elem (head startsymbol) (Set.toList parsedNonterminals) then head startsymbol else error "The start symbol is not from the list of nonterminals"
        parsedRules = makeTwoTuples (parseRules parsedNonterminals parsedTerminals rules)


-- -----------------------------
-- -----------------------------
-- -----------------------------
-- -----------------------------
-- -----------------------------
parseRules :: Set.Set Nonterminal -> Set.Set Terminal -> [String] -> [[String]]
parseRules n t rs
				| validateRules n t rulesList = rulesList
				| otherwise = error "Rules are not valid"
				where rulesList = map (splitOn "->") rs

validateRules :: Set.Set Nonterminal -> Set.Set Terminal -> [[String]] -> Bool
validateRules _ _ [] = True
validateRules n t (x:xs)
					| length x /= 2 || not (checkAllItems (x!!0) (Set.toList n)) || not (checkAllItems (x!!1) (Set.toList (n `Set.union` t `Set.union` (Set.fromList ['#'])))) = False
					| otherwise = validateRules n t xs


parseNonterminals :: [Char] -> Set.Set Nonterminal
parseNonterminals xs
                   | (areNonterminals nonterminalsList) = Set.fromList (map head nonterminalsList)
                   | otherwise = error "Set of nonterminals is incorrect"
                   where nonterminalsList = Set.toList (Set.fromList (splitOn "," xs))

parseTerminals :: [Char] -> Set.Set Terminal
parseTerminals xs
                   | (areTerminals terminalsList) = Set.fromList (map head terminalsList)
                   | otherwise = error "Set of terminals is incorrect"
                   where terminalsList = Set.toList (Set.fromList (splitOn "," xs))

-- the list must contain only string of length 1 and all strings must be uppercase
-- A         ------> True
-- A,B,C,D   ------> True
-- A,BCD,EFG ------> False
-- a,b,c,d   ------> False
-- ahoj      ------> False
areNonterminals :: [String] -> Bool
areNonterminals [] = True
areNonterminals (x:xs) = if length x == 1 && isUpper (head x)
	                        then areNonterminals xs
	                        else False


-- the list must contain only string of length 1 and all strings must be lowercase
-- a         ------> True
-- a,b,c,d   ------> True
-- a,bcd,efg ------> False
-- a,b,c,d   ------> False
-- ahoj      ------> False
areTerminals :: [String] -> Bool
areTerminals [] = True
areTerminals (x:xs) = if length x == 1 && isLower (head x)
	                        then areTerminals xs
	                        else False

--gets string content from stdin/file
-- borrowed from https://github.com/HonzaHK/flp1/blob/master/dka-2-mka.hs
getInput :: [Char] -> IO [Char]
getInput "" = getContents
getInput filename = readFile filename

-- [["A","f"],["B","g"],["C","h"],["D","i"],["E","j"]] ~~~~~~~~~~~> [('A',"f"),('B',"g"),('C',"h"),('D',"i"),('E',"j")]
makeTwoTuples :: [[String]] -> [(Char,String)]
makeTwoTuples [] = []
makeTwoTuples (x:xs) = [(x!!0!!0, x!!1)] ++ (makeTwoTuples xs)

-- Gets the list of pairs and returns the list of first parts of pairs
getFirstElements :: [(a,b)] -> [a]
getFirstElements xs = [y | (y,_) <- xs]

-- checkAllItems [1,2,3,4,5] [1..10] ~~~~~~~~~~~> True
-- checkAllItems [1,2,3,4,5,89] [1..10] ~~~~~~~~~~~> False
-- checkAllItems [1,2,3,89,4,5] [8..10] ~~~~~~~~~~~> False
checkAllItems :: (Eq a) => [a] -> [a] -> Bool
checkAllItems [] _ = True
checkAllItems (x:xs) y = elem x y && checkAllItems xs y

-- getNtSet ['A','B','S','D'] ['a','b'] [('A',"AB"),('B',"b"),('S',"A"),('S',"a"),('A',"a"),('A',"D"),('D',"A")] []
-- A,B,S,D
-- getNtSet ['A','B','S','D'] ['a','b'] [('A',"AB"),('B',"b"),('S',"A"),('S',"a"),('A',"D"),('D',"A")] []
-- B,S
getNtSet :: [Char] -> [Char] -> [(Char,String)] -> Set.Set Char -> Set.Set Char
getNtSet [] _ _ _ = Set.fromList []
getNtSet _ [] _ _ = Set.fromList []
getNtSet _ _ [] _ = Set.fromList []
getNtSet n t p prev 
                   | prev == new = new
                   | otherwise   = getNtSet n t p new
                   where new = Set.fromList ([ left | (left,right) <- p, checkAllItems right (t ++ Set.toList prev ++ ['#'])])

getRulesAsStrings :: [(Char,String)] -> [String]
getRulesAsStrings [] = []
getRulesAsStrings (x:xs) = [[left] ++ "->" ++ right] ++ getRulesAsStrings xs
							where left = fst x
							      right = snd x

getGStripe :: CFGrammar -> CFGrammar
getGStripe cf_grammar = CFGrammar {
	cfg_nonterminals = gstripe_nonterminals,
	cfg_terminals = gstripe_terminals,
	cfg_startSymbol = gstripe_startsymbol,
	cfg_rules = gstripe_rules
} where gstripe_nonterminals = Set.union (Set.fromList [cfg_startSymbol cf_grammar]) (getNtSet (Set.toList (cfg_nonterminals cf_grammar)) (Set.toList (cfg_terminals cf_grammar)) (cfg_rules cf_grammar) (Set.fromList []))
        gstripe_terminals = cfg_terminals cf_grammar
        gstripe_startsymbol = cfg_startSymbol cf_grammar
        gstripe_rules = filter (\n -> (elem (fst n) (Set.toList gstripe_nonterminals)) && (checkAllItems (snd n) (Set.toList (Set.union gstripe_terminals gstripe_nonterminals)))) (cfg_rules cf_grammar)

printGrammar :: CFGrammar -> IO ()
printGrammar grammar = do
	putStrLn (intersperse ',' (Set.toList (cfg_nonterminals grammar)))
	putStrLn (intersperse ',' (Set.toList (cfg_terminals grammar)))
	putStrLn [cfg_startSymbol grammar]
	putStrLn (intercalate  "\n" (getRulesAsStrings (cfg_rules grammar)))
