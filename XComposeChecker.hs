module XComposeChecker where

import Data.List (inits)
import qualified Data.ListTrie.Map as M
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Text.ParserCombinators.Parsec
import Text.Printf

type Event = String
type Keysym = String

data Target = Output String (Maybe Keysym)
    deriving (Eq)

instance Show Target where
    show (Output str (Just sym)) = printf "%s, %s" str sym
    show (Output str Nothing) = printf "%s" str

data XCompose = Sequence { events :: [Event], target :: [Target] }
    deriving (Eq)

instance Show XCompose where
    show (Sequence events target) = show events

extract (Sequence events target) = (events, target)

file :: Parser [XCompose]
file = catMaybes <$> group <?> "file"

group :: Parser [Maybe XCompose]
group = line `sepEndBy` spaces <?> "group"

keysym :: Parser Keysym
keysym = many1 (alphaNum <|> oneOf "*?_-.[]~=&:;!#$%^(){}") <?> "keysym_name"

key :: Parser Keysym
key = between (char '<') (char '>') keysym <?> "key"

keys :: Parser [Keysym]
keys = key `sepEndBy1` spaces <?> "keys"

res :: Parser Target
res = do
    str <- between (char '"') (char '"') (many1 $ satisfy (/= '"'))
    spaces
    sym <- option Nothing (Just <$> many1 alphaNum)
    return (Output str sym)

comment :: Parser (Maybe XCompose)
comment = do
            char '#'
            skipMany1 (satisfy (/= '\n'))
            return Nothing
        <?> "comment"

symEntry :: Parser (Maybe XCompose)
symEntry = do
            k <- keys
            char ':'
            spaces
            r <- res
            optional comment
            return $ Just (Sequence k [r])
        <?> "symEntry"

line :: Parser (Maybe XCompose)
line = symEntry <|> comment <?> "line"

constructTrie :: [XCompose] -> M.TrieMap Map.Map Event [Target]
constructTrie list = M.fromListWith (++) (map extract list)

type ResultTrie = M.TrieMap Map.Map Event [Target]

mshow list = M.showTrie list ""

duplicates :: ResultTrie -> String
duplicates = mshow . M.filter (\v -> length v /= 1)

prefixOverlap :: ResultTrie -> ResultTrie
prefixOverlap m = M.filterWithKey f m
    where f k _ = or [ M.member x m | x <- inits (init k) ]
