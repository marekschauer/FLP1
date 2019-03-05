import System.IO
import Data.List.Split


main :: IO ()
main = do
	putStrLn (getNtSet ['A','B','S','D'] ['a','b'] [('A',"AB"),('B',"b"),('S',"A"),('S',"a"),{-('A',"a"),-}('A',"D"),('D',"A")] [{-'B','S','A'-}])


getFirstElements :: [(a,b)] -> [a]
getFirstElements xs = [y | (y,_) <- xs]

-- checkAllItems [1,2,3,4,5] [1..10] ~~~~~~~~~~~> True
-- checkAllItems [1,2,3,4,5,89] [1..10] ~~~~~~~~~~~> False
-- checkAllItems [1,2,3,89,4,5] [8..10] ~~~~~~~~~~~> False
checkAllItems :: (Eq a) => [a] -> [a] -> Bool
checkAllItems [] _ = True
checkAllItems (x:xs) y = elem x y && checkAllItems xs y


getNtSet :: [Char] -> [Char] -> [(Char,String)] -> [Char] -> [Char]
getNtSet [] _ _ _ = []
getNtSet _ [] _ _ = []
getNtSet _ _ [] _ = []
getNtSet n t p prev 
                   | prev == new = new
                   | otherwise   = getNtSet n t p new
                   where new = [ left | (left,right) <- p, checkAllItems right t || checkAllItems right prev]
