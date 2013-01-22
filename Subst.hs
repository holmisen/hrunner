module Subst 
(subst)
where

import Data.Char (digitToInt, isDigit)
import Data.List (intersperse)


-- (subst s ps) returns s with variabels expanded to corresponding
-- parameters ps.
-- Variabels:
-- '$@'  substitute all parameters
-- '$n'  substitute the n:th parameter
-- '$+'  like '$@' but with '+' as separator
-- '@@'  substitute all parameters converted to html
-- '@n'  substitute the n:th parameter converted to html
-- '@+'  like '@@' but with '+' as separator
subst :: String -> [String] -> String
subst s ps = subst' s ps

subst' ""         ps = ""
subst' ('$':s)    ps = expand id     s ps
subst' ('@':s)    ps = expand toHtml s ps
subst' ('\\':c:s) ps = c : subst' s ps
subst' (c:s)      ps = c : subst' s ps


expand f ('@':s') ps = f (unwords ps) ++ subst' s' ps
expand f ('+':s') ps = f (concat $ intersperse "+" ps) ++ subst s' ps
expand f (d:s')   ps
   | isDigit d       = f (nth (digitToInt d) ps) ++ subst' s' ps
expand _ (c:s')   ps = c : subst' s' ps
expand _ s        ps = s


nth :: Int -> [String] -> String
nth n l
   | n < 0     = error "nth: negative index"
   | null l    = ""
   | n == 0    = head l
   | otherwise = nth (n-1) (tail l)


-- Convert a string to html string
toHtml :: String -> String
toHtml s = concat (map html s)

html 'Å' = "&Aring;"
html 'Ä' = "&Auml;"
html 'Ö' = "&Ouml;"
html 'å' = "&aring;"
html 'ä' = "&auml;"
html 'ö' = "&ouml;"
html '\'' = "&acute;"
html '"' = "&quot;"
html '&' = "&amp;"
html '<' = "&lt;"
html '>' = "&gt;"
html ' ' = "+"
html  c  = [c]
