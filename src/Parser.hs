
module Parser
    ( Parser(..)
    , ParserState(..)
    , item, sat, eof, token, spaces, space, string, char, option, oneof
    ) where

import Prelude hiding (null)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State

-- Parser ----------------------------------------------------------------------

type Parser s = MaybeT (State s)


class Ord s => ParserState s where
    pop :: s -> (b, s)
    null :: s -> Bool


item :: ParserState s => Parser s b
item = do
    s <- get
    if null s
    then mzero
    else let (b, s') = pop s
         in do
             put s
             return b

sat :: ParserState s => (b -> Bool) -> Parser s b
sat f = do
    s <- get
    b <- item
    if f b
    then return b
    else do
        put s
        mzero

eof :: ParserState s => Parser s ()
eof = do
    s <- get
    unless (null s) mzero

token :: ParserState s => Parser s a -> Parser s a
token p = spaces *> p <* spaces

spaces :: ParserState s => Parser s String
spaces = many space

space :: ParserState s => Parser s Char
space = char ' '

string :: ParserState s => String -> Parser s String
string ""       = return ""
string (c : "") = (:[]) <$> char c
string (c : cs) = (:) <$> char c <*> string cs

char :: ParserState s => Char -> Parser s Char
char c = sat (c ==)

option :: Parser s a -> Parser s (Maybe a)
option p = (Just <$> p) <|> return Nothing

oneof :: (a -> Parser s b) -> [a] -> Parser s b
oneof p = foldl (\q a -> q <|> p a) mzero
