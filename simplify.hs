import System.IO
import System.Environment
import Data.List.Split
import Data.Char
import qualified Data.Set as Set



main :: IO ()
main = do
	inputStr <- getContents
	let cf_grammar = parseInput (lines inputStr)
	print cf_grammar


type Nonterminal = Char
type Terminal = Char
type StartSymbol = Char

data CFGrammar = CFGrammar {
    cfg_nonterminals::Set.Set Nonterminal,
    cfg_terminals::Set.Set Terminal,
    cfg_startSymbol::StartSymbol,
    cfg_rules::[[String]]
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
        parsedRules = parseRules parsedNonterminals parsedTerminals rules


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
					| length x /= 2 || not (checkAllItems (x!!0) (Set.toList n)) || not (checkAllItems (x!!1) (Set.toList (n `Set.union` t))) = False
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
                   where new = Set.fromList ([ left | (left,right) <- p, checkAllItems right (t ++ Set.toList prev)])
