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

padded :: Parsec Text u a -> Parsec Text u a
padded parser = do
    spaces
    res <- parser
    spaces
    return res

-- Pair syntax (car . cdr)
pair :: Parsec Text u SData
pair = do
    char '('
    car <- padded sexpr
    char '.'
    cdr <- padded sexpr
    char ')'
    return $ SPair car cdr

-- Parse (1 2 3 4) into (1 . (2 . (3 . (4 . ()))))
list :: Parsec Text u SData
list = do
    char '('
    subs <- many (padded sexpr)
    char ')'
    return . foldr SPair SNil $ subs

atom :: Parsec Text u SData
atom = ident <|> bool <|> int <|> str <|> nil

int :: Parsec Text u SData
int = liftM (SInt . read) $ many1 digit

ident :: Parsec Text u SData
ident = do
    initial <- letter
    subsequent <- many (letter <|> digit)
    return . SIdent $ (initial:subsequent)

bool :: Parsec Text u SData
-- Binding to return is not very pretty; I should clean this up
bool = (try (string "#t") >> (return $ SBool True)) <|> (try (string "#f") >> (return $ SBool False))

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
