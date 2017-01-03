module PrefixTree
(Dict
,suggest
,suggestAny
,mkDict)
where

import Data.List     (find, groupBy, isPrefixOf, stripPrefix)
import Data.Map      (Map)
import Data.Maybe    (maybe)
import Data.Function (on)

import qualified Data.Map as Map


data Dict = Index (Map Char Dict)
          | Words [String]
   deriving (Show)


suggest :: Dict -> String -> Maybe String
suggest d@(Index m) s =
   case s of
      (c:cs) -> Map.lookup c m >>= \d -> suggest d cs
      []     -> Just (suggestAny d)
suggest (Words ws) p =
   find (isPrefixOf p) ws >>= stripPrefix p

suggestAny :: Dict -> String
suggestAny (Index m)  = let (c,d) = head $ Map.assocs m in c : suggestAny d
suggestAny (Words ws) = head ws


-- (mkDict n ws) create a dictionary with words ws indexed on the
-- n first characters.
-- n should not be to big -- 2 or 3 should suffice for a small set of
--   words.
-- ws must be sorted
mkDict :: Int -> [String] -> Dict
mkDict l ws
   | l < 1     = Words ws
   | otherwise = let gs = groupBy ((==) `on` head) (filter (not . null) ws)
                     as = map (\ws -> (head $ head ws,
                                       mkDict (l-1) (map tail ws))) gs
                 in Index $ Map.fromList as
