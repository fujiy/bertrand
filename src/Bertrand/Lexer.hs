
module Bertrand.Lexer
    (tokenize
    )where

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Bertrand.Data

import Debug.Trace


tokenize :: String -> Either ParseError Statement
tokenize xs = let (r, s) = runParser lexer $ purePS xs
              in case r of
                  Just st -> Right st
                  Nothing -> let ParserS _ p = s
                             in  Left $ ParseError UnusableChar p

lexer :: Parser Statement
lexer = statement

fixity :: Parser Statement
fixity = f <$> oneof word ["infixl", "infixr", "infixp", "infix"]
                <*> number <* spaces <*> operator <* spaces <* eof
    where
        f xs (NumL n) ys = Fixity (case xs of
                            "infix"  -> Infix
                            "infixl" -> InfixL
                            "infixr" -> InfixR
                            "infixp" -> InfixP) (fromIntegral n) (Name ys)


statement :: Parser Statement
statement = Statement <$> tokens

tokens :: Parser [Token]
tokens = ((++) <$> float <*> tokens)
     <|> ((:)  <$> token <*> tokens)
     <|> (const [] <$> spaces <* eof)

token :: Parser Token
token = bracket <|> number <|> name

bracket :: Parser Token
bracket = Name . (: []) <$> oneof char "()[]{}"

float :: Parser [Token]
float = f <$> (spaces *> some digit) <* char '.' <*> some digit
    where
        f xs ys = [Name "(", NumL . read $ xs ++ ys, Name "/", NumL $ 10 ^ length ys, Name ")"]

number :: Parser Token
number = NumL . read <$> (spaces *> some digit)

name :: Parser Token
name = Name <$> (spaces *> (identifer <|> operator))

identifer :: Parser String
identifer = (:) <$> letter <*> many (letter <|> digit)

operator :: Parser String
operator = some (oneof char ".$~=<>/:!?+-*^|")

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

word :: String -> Parser String
word s = spaces *> string s

spaces :: Parser String
spaces = many space

space :: Parser Char
space = char ' '

string :: String -> Parser String
string ""       = return ""
string (c : "") = (:[]) <$> char c
string (c : cs) = (:) <$> char c <*> string cs

char :: Char -> Parser Char
char c = sat (c ==)

option :: Parser a -> Parser (Maybe a)
option p = (Just <$> p) <|> return Nothing

oneof :: (a -> Parser b) -> [a] -> Parser b
oneof p = foldl (\q a -> q <|> p a) mzero

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= check
    where
        check c | f c = return c
        check _       = mzero

-- (<|>) :: Parser a -> Parser a -> Parser a
-- p <|> q = MaybeT $ do
--         s <- get
--         v <- runMaybeT p
--         case traceShow s v of
--             Nothing -> do
--                 put s
--                 s' <- get
--                 traceShow s' $ runMaybeT q
--             Just _  -> return v

-- item :: Parser Char
-- item = do
--     ParserS xs (l, m) <- lift get
--     case xs of
--         ""     -> empty
--         (y:ys) -> do
--             lift $ put $ ParserS ys (l, m + 1)
--             return y
--
-- eof :: Parser ()
-- eof = do
--     ParserS xs _ <- lift get
--     case xs of
--         ""    -> return ()
--         (_:_) -> mzero
--
-- test :: Parser Char
-- test = do
--     ParserS xs _ <- lift get
--     case xs of
--         ""    -> return '\0'
--         (y:_) -> return y
