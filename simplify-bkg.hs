-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
-- FLP Project 
-- Author: Marek Schauer (xschau00)
-- Course: FLP - functional and logic programming
-- Project: simplify-bkg
-- Academic year: 2018/2019
-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------

import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as Set

type Nonterminal = Char
type Terminal = Char
type StartSymbol = Char

data CFGrammar = CFGrammar {
    cfgNonterminals::Set.Set Nonterminal,
    cfgTerminals::Set.Set Terminal,
    cfgStartSymbol::StartSymbol,
    cfgRules::[(Char,String)]
} deriving (Show)


main :: IO ()
main = do
	arguments <- getArgs
	inputStr <- getInput (getInputFileName arguments) -- getContents
	let cf_grammar = parseInput (lines inputStr)
	let resultGrammar = processInputGrammar cf_grammar arguments
	printGrammar resultGrammar
	
-- gets the input file name, if the was no input file name entered, returns empty string
getInputFileName :: [String] -> String
getInputFileName [] = ""
getInputFileName (x:xs)
                   | x `notElem` ["-i", "-1", "-2"] = x
                   | otherwise = getInputFileName xs

-- Reads the grammar from list of lines
parseInput :: [String] -> CFGrammar 
parseInput (nonterminals:terminals:startsymbol:rules) = CFGrammar {
	cfgNonterminals = parsedNonterminals,
	cfgTerminals = parsedTerminals,
	cfgStartSymbol = startsymbolChecked,
	cfgRules = parsedRules
} where parsedNonterminals = parseNonterminals nonterminals
        parsedTerminals = parseTerminals terminals
        startsymbolChecked = if head startsymbol `elem` Set.toList parsedNonterminals 
                             then head startsymbol 
                             else error "The start symbol is not from the list of nonterminals"
        parsedRules = makeTwoTuples (parseRules parsedNonterminals parsedTerminals rules)

-- Processes the given grammar depending on given arguments
processInputGrammar :: CFGrammar -> [String] -> CFGrammar
processInputGrammar g a
					| "-i" `elem` a = g
					| "-1" `elem` a = if isTheLanguageNonEmpty g 
                            then getGStripe g 
                            else error "The input grammar generates empty language"
					| "-2" `elem` a = if isTheLanguageNonEmpty g
                            then withoutUnreachableSymbols g 
                            else error "The input grammar generates empty language"
					| otherwise   = error "The program must be launched with one of these parameters: -i, -1, -2"

-- Parses rules and checks following things:
-- 	* detects, whether the grammar is empty?
-- 	* validates rules
-- 	* are all the rules unique?
parseRules :: Set.Set Nonterminal -> Set.Set Terminal -> [String] -> [[String]]
parseRules n t rs
				| null rulesList = error "The set of rules of input grammar cannot be empty"
				| not (validateRules n t rulesList) = error "Rules are not valid"
				| not (allUnique' rulesList)        = error "Set of rules of input grammar contains duplicate rules"
				| otherwise = rulesList
				where rulesList = map (splitOn "->") rs

-- Validates rules, checks following things:
-- 	* do all rules contain just one '->'?
-- 	* is the right side of rule composed only by terminals & nonterminals?
-- 	* is the right side of the rule nonterminal?
validateRules :: Set.Set Nonterminal -> Set.Set Terminal -> [[String]] -> Bool
validateRules _ _ [] = True
validateRules n t (x:xs)
					| length x /= 2 = False
					| '#' `elem` (x !! 1) && (length (x !! 1) > 1) = False
					| not (checkAllItems (head x) (Set.toList n)) = False
					| not (checkAllItems (x!!1) terminals) = False
					| otherwise = validateRules n t xs
					where terminals = Set.toList (n `Set.union` t `Set.union` Set.fromList "#")

-- Gets the nonterminals (checks for validity as well)
parseNonterminals :: String -> Set.Set Nonterminal
parseNonterminals xs
                   | not (areNonterminals uniqueNonterminalsList) 
                   						= error "Set of nonterminals is incorrect"
                   | not (allUnique' nonterminalsList)            
                   						= error "Set of nonterminals contains duplicates"
                   | otherwise          = Set.fromList (map head uniqueNonterminalsList)
                   where uniqueNonterminalsList = Set.toList (Set.fromList (splitOn "," xs))
                         nonterminalsList       = splitOn "," xs

-- checks whether all elements of the list are unique
-- 		allUnique' "foo bar" == False
-- 		allUnique' ['a'..'z'] == True
-- 		allUnique' [] == True (!)
allUnique' :: (Eq a) => [a] -> Bool
allUnique' [] = True
allUnique' (x:xs)
                 | x `elem` xs = False
                 | otherwise   = allUnique' xs

-- Gets the terminals (checks for validity as well)
parseTerminals :: String -> Set.Set Terminal
parseTerminals xs
                   | null xs = Set.fromList []
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
areNonterminals
    = foldr (\ x -> (&&) (length x == 1 && isUpper (head x))) True

-- the list must contain only strings of length 1 and all strings must be lowercase
-- a         ------> True
-- a,b,c,d   ------> True
-- a,bcd,efg ------> False
-- a,b,c,d   ------> False
-- ahoj      ------> False
areTerminals :: [String] -> Bool
areTerminals
    = foldr (\ x -> (&&) (length x == 1 && isLower (head x))) True

--gets string content from stdin/file
-- borrowed from https://github.com/HonzaHK/flp1/blob/master/dka-2-mka.hs
getInput :: String -> IO String
getInput "" = getContents
getInput filename = readFile filename

-- Makes two-tuples from list of lists (the inner list must contain
-- just the two elements)
-- [["A","f"],["B","g"],["C","h"],["D","i"],["E","j"]] ~~> [('A',"f"),('B',"g"),('C',"h"),('D',"i"),('E',"j")]
makeTwoTuples :: [[String]] -> [(Char,String)]
makeTwoTuples = foldr (\ x -> (++) [(head (head x), x !! 1)]) []

-- Gets the list of pairs and returns the list of first parts of pairs
getFirstElements :: [(a,b)] -> [a]
getFirstElements xs = [y | (y,_) <- xs]

-- Checks, whether all elements of the first list are contained in second list
-- checkAllItems [1,2,3,4,5] [1..10] ~~~~~~~~~~~> True
-- checkAllItems [1,2,3,4,5,89] [1..10] ~~~~~~~~~~~> False
-- checkAllItems [1,2,3,89,4,5] [8..10] ~~~~~~~~~~~> False
checkAllItems :: (Eq a) => [a] -> [a] -> Bool
checkAllItems [] _ = True
checkAllItems (x:xs) y = elem x y && checkAllItems xs y

-- Gets the Nt set from algorithm 4.1
getNtSet :: String ->
    String -> [(Char, String)] -> Set.Set Char -> Set.Set Char
getNtSet [] _ _ _ = Set.fromList []
getNtSet _ [] _ _ = Set.fromList []
getNtSet _ _ [] _ = Set.fromList []
getNtSet n t p prev 
                   | prev == new = new
                   | otherwise   = getNtSet n t p new
                   where new = Set.fromList [left | (left, right) <- p,
                                             checkAllItems right (t ++ Set.toList prev ++ "#")]

-- Gets the final Vi set from algorithm 4.2
getViSet :: String ->
    String -> [(Char, String)] -> Set.Set Char -> Set.Set Char
getViSet [] _ _ _ = Set.fromList []
getViSet _ [] _ _ = Set.fromList []
getViSet _ _ [] _ = Set.fromList []
getViSet n t p prev
                   | new == prev = new
                   | otherwise   = getViSet n t p new
                   where new = prev `Set.union`
                                Set.fromList (concat [x | (a, x) <- p, a `elem` Set.toList prev])

-- Converts the rules to their string representation
getRulesAsStrings :: [(Char,String)] -> [String]
getRulesAsStrings [] = []
getRulesAsStrings (x:xs) = ([left] ++ "->" ++ right) : getRulesAsStrings xs
							where left = fst x
							      right = snd x

-- Detects, whether the language generated by grammar is non-empty
isTheLanguageNonEmpty :: CFGrammar -> Bool
isTheLanguageNonEmpty g = cfgStartSymbol g `elem` ntlist
                          where  nonterminals = Set.toList (cfgNonterminals g)
                                 terminals = Set.toList (cfgTerminals g)
                                 rules = cfgRules g
                                 emptySet = Set.fromList []
                                 ntlist = Set.toList (getNtSet nonterminals terminals rules emptySet)

-- Gets the set G with stripe from algorithm 4.3
getGStripe :: CFGrammar -> CFGrammar
getGStripe cf_grammar = CFGrammar {
	cfgNonterminals = gstripe_nonterminals,
	cfgTerminals = gstripe_terminals,
	cfgStartSymbol = gstripe_startsymbol,
	cfgRules = gstripe_rules
} where startSymbolSet = Set.fromList [cfgStartSymbol cf_grammar]
        nonterminals = Set.toList (cfgNonterminals cf_grammar)
        terminals = Set.toList (cfgTerminals cf_grammar)
        rules = cfgRules cf_grammar
        ntSet = getNtSet nonterminals terminals rules (Set.fromList [])
        gstripe_nonterminals = startSymbolSet `Set.union` ntSet
        gstripe_nonterminals_list = Set.toList gstripe_nonterminals
        gstripe_terminals = cfgTerminals cf_grammar
        gstripe_startsymbol = cfgStartSymbol cf_grammar
        terminals_nonterminals_hashtag = Set.toList (gstripe_terminals `Set.union` gstripe_nonterminals) ++ "#"
        gstripe_rules = filter (\n -> elem (fst n) gstripe_nonterminals_list && checkAllItems (snd n) terminals_nonterminals_hashtag) rules


-- Converts the given grammar to grammar without unreachable symbols
-- The algorithm used for conversion is the Algorithm 4.3
withoutUnreachableSymbols :: CFGrammar -> CFGrammar
withoutUnreachableSymbols g = CFGrammar {
	cfgNonterminals = new_nonterminals,
	cfgTerminals    = new_terminals,
	cfgStartSymbol  = new_startsymbol,
	cfgRules        = new_rules
} where gStripeGrammar   = getGStripe g
        nonterminals 	 = Set.toList (cfgNonterminals gStripeGrammar)
        terminals        = Set.toList (cfgTerminals gStripeGrammar)
        rules            = cfgRules gStripeGrammar
        startsymbol      = cfgStartSymbol gStripeGrammar
        viSet            = getViSet nonterminals terminals rules (Set.fromList [startsymbol])
        new_nonterminals = Set.intersection viSet (cfgNonterminals gStripeGrammar)
        new_terminals    = Set.intersection viSet (cfgTerminals gStripeGrammar)
        new_startsymbol  = cfgStartSymbol gStripeGrammar
        new_rules        = [ (l,r) | (l,r) <- rules, l `elem` Set.toList viSet, checkAllItems r (Set.toList viSet)]


-- Prints the given grammar
printGrammar :: CFGrammar -> IO ()
printGrammar grammar = do
	putStrLn (intersperse ',' (Set.toList (cfgNonterminals grammar)))
	putStrLn (intersperse ',' (Set.toList (cfgTerminals grammar)))
	putStrLn [cfgStartSymbol grammar]
	putStrLn (intercalate  "\n" (getRulesAsStrings (cfgRules grammar)))
