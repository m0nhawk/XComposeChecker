module XComposeChecker where

import Data.List(inits)
import qualified Data.ListTrie.Map as M
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

type Event = String
type Keysym = String

data Target = Output String (Maybe Keysym)
    deriving (Eq, Show)

data XCompose = Sequence { events :: [Event], target :: [Target] }
    deriving (Eq, Show)

extract (Sequence events target) = (events, target)

file :: Parser [XCompose]
file = do
        g <- group
        eof
        return g

group :: Parser [XCompose]
group = line `sepEndBy` spaces <?> "group"

keysym :: Parser Keysym
keysym = many1 (alphaNum <|> oneOf "*?_-.[]~=&:;!#$%^(){}") <?> "keysym_name"

key :: Parser Keysym
key = do
        spaces
        k <- between (char '<') (char '>') keysym
        spaces
        return k
    <?> "key"

keys :: Parser [Keysym]
keys = many1 key <?> "keys"

res :: Parser Target
res = do
        str <- between (char '"') (char '"') (many1 $ satisfy (/= '"'))
        spaces
        sym <- option Nothing (fmap Just keysym)
        spaces
        return (Output str sym)

comment :: Parser ()
comment = do
            char '#'
            skipMany (satisfy (/= '\n'))
        <?> "comment"

line :: Parser XCompose
line = do
        k <- keys
        spaces
        char ':'
        spaces
        r <- res
        option Nothing (fmap Just comment)
        return (Sequence k [r])
    <?> "line"

constructTrie :: [XCompose] -> M.TrieMap Map.Map Event [Target]
constructTrie list = M.fromListWith (++) (map extract list)

type ResultTrie = M.TrieMap Map.Map Event [Target]

mshow list = M.showTrie list ""

duplicates :: ResultTrie -> String
duplicates = mshow . M.filter (\v -> length v /= 1)

prefixOverlap :: ResultTrie -> ResultTrie
prefixOverlap m = M.filterWithKey f m
  where
    f k _ = or [M.member x m | x <- inits (init k)]
