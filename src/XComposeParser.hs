module XComposeParser where

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec

import XComposeTypes

keysym :: Parser Keysym
keysym = many1 (alphaNum <|> oneOf "*?_-.[]~=&:;!#$%^(){}") <?> "keysym_name"

key :: Parser Keysym
key = between (char '<') (char '>') keysym <?> "key"

keys :: Parser [Keysym]
keys = key `sepEndBy1` spaces <?> "keys"

res :: Parser Target
res = Output <$> event <*> keysym <?> "res"
    where
        event = between (char '"') (char '"') (many1 $ satisfy (/= '"')) <* spaces
        keysym = option Nothing (Just <$> many1 alphaNum)

comment :: Parser (Maybe XCompose)
comment = ignoreComment >> return Nothing <?> "comment"
    where
        ignoreComment = char '#' *> skipMany1 (satisfy (/= '\n'))

line :: Parser (Maybe XCompose)
line = symEntry <|> comment <?> "line"
    where
        k = keys <* (char ':' *> spaces)
        r = res <* optional comment
        symEntry = Just <$> (Sequence <$> k <*> r) <?> "symEntry"

group :: Parser [Maybe XCompose]
group = line `sepEndBy` spaces <?> "group"

file :: Parser [XCompose]
file = catMaybes <$> group <?> "file"
