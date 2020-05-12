module Completion
   ( Dict
   , makeDict
   , getSuffixes
   )
where

import Data.Maybe (catMaybes)
import Data.List (stripPrefix, isPrefixOf, sort, nub, reverse)

--------------------------------------------------------------------------------

data Dict = Dict [String] deriving Show


makeDict :: [String] -> Dict
makeDict = Dict . nub . reverse


getSuffixes :: Dict -> String -> [String]
getSuffixes (Dict ws) p =
   catMaybes $ map (stripPrefix p) ws


sortUniq :: Ord a => [a] -> [a]
sortUniq = uniq . sort where
   uniq (x : xs@(y : _))
      | x == y    = uniq xs
      | otherwise = x : uniq xs
   uniq xs = xs
