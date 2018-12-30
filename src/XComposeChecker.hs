module XComposeChecker where

import Data.List (inits, nub)
import qualified Data.ListTrie.Map as M
import qualified Data.Map as Map

import XComposeTypes
import XComposeParser

constructTrie :: [XCompose] -> M.TrieMap Map.Map Event [Target]
constructTrie list = M.fromListWith (++) (map extract list)

type ResultTrie = M.TrieMap Map.Map Event [Target]

mshow :: ResultTrie -> String
mshow list = M.showTrie list ""

duplicates :: ResultTrie -> String
duplicates = mshow . M.filter (\v -> length v /= 1)

prefixOverlap :: ResultTrie -> ResultTrie
prefixOverlap m = M.filterWithKey f m
    where
        f k _ = or [ x `M.member` m | x <- allNonemptyPrefixes k ]
        allPrefixes = inits . init
        allNonemptyPrefixes k = filter (not . null) (allPrefixes k)

dupKeys :: ResultTrie -> [[Event]]
dupKeys m = nub $ concatMap (inits . (init . fst)) (M.toList $ prefixOverlap m)
