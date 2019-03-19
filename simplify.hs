import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as Set



-- TODO
--	- pri prepinacoch -1 a -2 kontrolovat, ci to generuje neprazdny jazyk
--	- zoznam pravidiel nemoze byt prazdny
--	- pravidlo nemoze byt napr. A -> abcd#mdks


main :: IO ()
main = do
	arguments <- getArgs
	inputStr <- getInput (getInputFileName arguments) -- getContents
	-- print inputStr
	let cf_grammar = parseInput (lines inputStr)
	-- print  (makeTwoTuples (cfg_rules cf_grammar))
	-- print (makeTwoTuples (Set.toList (cfg_rules cf_grammar)))
	-- putStrLn (getInputFileName arguments)
	let resultGrammar = processInputGrammar cf_grammar arguments
	printGrammar resultGrammar
	-- print (isTheLanguageNonEmpty cf_grammar)
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

processInputGrammar :: CFGrammar -> [String] -> CFGrammar
processInputGrammar g a
					| elem "-i" a = g
					| elem "-1" a = if isTheLanguageNonEmpty g then getGStripe g else error "The input grammar generates empty language"
					| elem "-2" a = if isTheLanguageNonEmpty g then g else error "The input grammar generates empty language"
					| otherwise   = error "The program must be launched with one of these parameters: -i, -1, -2"
-- -----------------------------
-- -----------------------------
-- -----------------------------
-- -----------------------------
-- -----------------------------
parseRules :: Set.Set Nonterminal -> Set.Set Terminal -> [String] -> [[String]]
parseRules n t rs
				| not ((length rulesList) > 0) = error "The set of rules of input grammar cannot be empty"
				| not (validateRules n t rulesList) = error "Rules are not valid"
				| not (allUnique' rulesList)        = error "Set of rules of input grammar contains duplicate rules"
				| otherwise = rulesList
				where rulesList = map (splitOn "->") rs

validateRules :: Set.Set Nonterminal -> Set.Set Terminal -> [[String]] -> Bool
validateRules _ _ [] = True
validateRules n t (x:xs)
					| length x /= 2 = False
					| not (checkAllItems (x!!0) (Set.toList n)) = False
					| not (checkAllItems (x!!1) (Set.toList (n `Set.union` t `Set.union` (Set.fromList ['#'])))) = False
					| otherwise = validateRules n t xs


parseNonterminals :: [Char] -> Set.Set Nonterminal
parseNonterminals xs
                   | not (areNonterminals uniqueNonterminalsList) = error "Set of nonterminals is incorrect"
                   | not (allUnique' nonterminalsList)            = error "Set of nonterminals contains duplicates"
                   | otherwise                                    = Set.fromList (map head uniqueNonterminalsList)
                   where uniqueNonterminalsList = Set.toList (Set.fromList (splitOn "," xs))
                         nonterminalsList       = splitOn "," xs


-- checks whether all elements of the list are unique
-- 		allUnique' "foo bar" == False
-- 		allUnique' ['a'..'z'] == True
-- 		allUnique' [] == True (!)
allUnique' :: (Eq a) => [a] -> Bool
allUnique' [] = True
allUnique' (x:xs)
                 | elem x xs = False
                 | otherwise = allUnique' xs

parseTerminals :: [Char] -> Set.Set Terminal
parseTerminals xs
                   | not (areTerminals uniqueTerminalsList) = error "Set of terminals is incorrect"
                   | not (allUnique' terminalsList)   = error "Set of terminals contains duplicates"
                   | otherwise                        = Set.fromList (map head uniqueTerminalsList)
                   where uniqueTerminalsList = Set.toList (Set.fromList (splitOn "," xs))
                         terminalsList       = splitOn "," xs

-- the list must contain only string of length 1 and all strings must be uppercase
-- 		A         ------> True
-- 		A,B,C,D   ------> True
-- 		A,BCD,EFG ------> False
-- 		a,b,c,d   ------> False
-- 		ahoj      ------> False
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

-- get final Vi set from algorithm 4.2
-- getVSet :: [Char] -> [Char] -> [(Char,String)] -> Set.Set Char -> Set.Set Char
-- getVSet [] _ _ _ = Set.fromList []
-- getVSet _ [] _ _ = Set.fromList []
-- getVSet _ _ [] _ = Set.fromList []
-- getVSet n t p prev
--                    | prev == new = new
--                    | otherwise   = getVSet n t p new
--                    where new = Set.fromList [ head rightSide | (leftSide, rightSide) <- p, elem leftSide (Set.toList prev) ]
                   			-- where tmpxsd = [ (leftSide, rightSide) | (leftSide, rightSide) <- p, elem leftSide (Set.toList prev) ]


getRulesAsStrings :: [(Char,String)] -> [String]
getRulesAsStrings [] = []
getRulesAsStrings (x:xs) = [[left] ++ "->" ++ right] ++ getRulesAsStrings xs
							where left = fst x
							      right = snd x

isTheLanguageNonEmpty :: CFGrammar -> Bool
isTheLanguageNonEmpty g = if elem (cfg_startSymbol g) (Set.toList (getNtSet (Set.toList (cfg_nonterminals g)) (Set.toList (cfg_terminals g)) (cfg_rules g) (Set.fromList [])))
							then True
							else False

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
