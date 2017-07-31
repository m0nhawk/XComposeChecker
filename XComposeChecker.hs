import Text.ParserCombinators.Parsec

data Target = Output String (Maybe Keysym)
    deriving (Show)

data XCompose = Sequence [Keysym] Target
    deriving (Show)

type Keysym = String

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
        return (Sequence k r)
    <?> "line"
