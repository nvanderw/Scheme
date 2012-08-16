module Language.Scheme.Parser (SData (..),
                               sexpr)
where


import Control.Monad
import Text.Parsec
import Text.Parsec.Text.Lazy
import Data.Text.Lazy (Text)
import Language.Scheme.Types

sexpr :: Parsec Text u SData 
sexpr = quoted <|> (try atom) <|> (try pair) <|> (try list)

-- Pair syntax (car . cdr)
pair :: Parsec Text u SData
pair = do
    char '('
    car <- sexpr
    spaces
    char '.'
    spaces
    cdr <- sexpr
    char ')'
    return $ SPair car cdr

-- Parse (1 2 3 4) into (1 . (2 . (3 . (4 . ()))))
list :: Parsec Text u SData
list = do
    char '('
    subs <- sexpr `sepBy1` (many1 space)
    char ')'
    return . foldr SPair SNil $ subs

atom :: Parsec Text u SData
atom = ident <|> int <|> str <|> nil

int :: Parsec Text u SData
int = liftM (SInt . read) $ many1 digit

ident :: Parsec Text u SData
ident = do
    initial <- letter
    subsequent <- many (letter <|> digit)
    return . SIdent $ (initial:subsequent)

str :: Parsec Text u SData
str = do
    char '"'
    s <- many1 $ satisfy (/= '"')
    char '"'
    return . SString $ s

nil :: Parsec Text u SData
nil = char '(' >> char ')' >> return SNil

quoted :: Parsec Text u SData
quoted = char '\'' >> (liftM SQuote sexpr)
