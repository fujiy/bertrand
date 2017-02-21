{-# LANGUAGE LambdaCase #-}

module Bertrand.Parser
    (parse
    )where

import Prelude hiding (null)
import Control.Applicative hiding (Const)
import Control.Monad
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.State
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

import Bertrand.Data


--------------------------------------------------------------------------------

-- type Parser a = MaybeT (State ParserState) a

-- runParser = runMaybeT

data Parser a = Parser {runParser :: ParserState -> (Maybe a, ParserState)}

instance Functor Parser where
    f `fmap` p = Parser $ \s -> let (m, s') = runParser p s
                                in (f `fmap` m, s')

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return x = Parser $ \s -> (return x, s)
    p >>= f  = Parser $ \s -> let (m, s') = runParser p s
                              in maybe (empty, s) (\a -> runParser (f a) s') m

instance Alternative Parser where
    empty = Parser $ \s -> (empty, s)

    p <|> q = Parser $ \s -> let a@(m, s') = runParser p s
                             in maybe (runParser q s) (const a) m

instance MonadPlus Parser

get :: Parser ParserState
get = Parser $ \s -> (return s, s)

put :: ParserState -> Parser ()
put s = Parser $ const (return (), s)


data ParserState = ParserState (Int, Int) String

purePS :: String -> ParserState
purePS = ParserState (1, 1)

pop :: ParserState -> (Char, ParserState)
pop (ParserState (i, j) s) = case s of
    []      -> ('\0', ParserState (i, j) s)
    '\n':cs -> ('\n', ParserState (i + 1, j) cs)
    c   :cs -> (c,    ParserState (i, j + 1) cs)

null :: ParserState -> Bool
null (ParserState _ s)
    | s == ""   = True
    | otherwise = False


item :: Parser Char
item = do
    s <- get
    if null s
    then mzero
    else let (c, s') = pop s
         in do
             put s'
             return c

sat :: (Char -> Bool) -> Parser Char
sat f = do
    s <- get
    c <- item
    if f c
    then return c
    else do
        put s
        mzero

test :: Parser a -> Parser a
test p = do
    s <- get
    a <- p
    put s
    return a

notp :: Parser a -> Parser ()
notp p = Parser $ \s -> let (m, s') = runParser p s
                        in maybe (return (), s) (const (mzero, s)) m

eof :: Parser ()
eof = do
    s <- get
    unless (null s) mzero

--------------------------------------------------------------------------------

parse :: [ParseOption] -> String -> Either (Int, Int) Expr
parse os cs = let (m, ParserState p _) = runParser (parser os) (purePS cs)
              in  maybe (Left p) Right m

--------------------------------------------------------------------------------

type OpeParser = Parser Expr -> Parser Expr

parser :: [ParseOption] -> Parser Expr
parser ops = toData ops <$> expr <* eof
    where
        expr :: Parser Expr
        expr = foldr id (apply signs expr) opeparsers
        -- expr = apply $ head opeparsers $ term expr


        opeparsers :: [OpeParser]
        opeparsers = declaration : bind : lambda (term signs expr) : map opeparser opers

        opers = operators ops

        signs :: [String]
        signs = "!":"=":";":"\\":"->":",": foldr (\(is, ils, irs, ifs) s -> is ++ ils ++ irs ++ ifs ++ s) [] opers

toData :: [ParseOption] -> Expr -> Expr
toData ops = \case
    Const s -> if s `elem` consts then Data s [] else Const s
    a       -> emap (toData ops) a
    where
        consts :: [String]
        consts = mapMaybe (\case
            DataCons s -> Just s
            _          -> Nothing) ops

        -- replaceCons :: Expr -> Expr
        -- replaceCons a = case appToList a of
        --     Const s : es | s `elem` dataconses
        --         -> Cons s (map replaceCons es)
        --     _   -> emap replaceCons a

appToList :: Expr -> [Expr]
appToList (App a b) = appToList a ++ [b]
appToList a         = [a]

listToApp :: [Expr] -> Expr
listToApp = foldl1 App


operators :: [ParseOption] -> [([String], [String], [String], [String])]
operators = M.elems . foldr f M.empty
    where
        f :: ParseOption -> M.Map Int ([String], [String], [String], [String]) -> M.Map Int ([String], [String], [String], [String])
        f (Infix  i s) = M.insertWith (const $ map1st (s:)) i ([s], [], [], [])
        f (Infixl i s) = M.insertWith (const $ map2nd (s:)) i ([], [s], [], [])
        f (Infixr i s) = M.insertWith (const $ map3rd (s:)) i ([], [], [s], [])
        f (Infixf i s) = M.insertWith (const $ map4th (s:)) i ([], [], [], [s])
        f _ = id

        map1st f (a, b, c, d) = (f a, b, c, d)
        map2nd f (a, b, c, d) = (a, f b, c, d)
        map3rd f (a, b, c, d) = (a, b, f c, d)
        map4th f (a, b, c, d) = (a, b, c, f d)

--            infix     infixl    infixr    infixf
opeparser :: ([String], [String], [String], [String]) -> OpeParser
opeparser (is, ils, irs, ifs) p = infixp $ infixlp $ infixrp $ infixfp p
    -- p <*> some (oneof ils )


    where
        infixp :: OpeParser
        infixp p = f <$> p <*> option ((,) <$> oper is <*> option p)
            where
                f a Nothing             = a
                f a (Just (o, Nothing)) = App o a
                f a (Just (o, Just b))  = App (App o a) b

        infixlp :: OpeParser
        infixlp p = f <$> g
            where
                f :: [Expr] -> Expr
                f [a]       = a
                f [a, o]    = App o a
                f (a:o:b:x) = f $ App (App o a) b : x
                g :: Parser [Expr]
                g = (:) <$> p <*> (concat <$> optionL ((:) <$> oper ils <*> (concat <$> optionL g)))

        infixrp :: OpeParser
        infixrp p = f <$> p <*> option ((,) <$> oper irs <*> option (infixrp p))
            where
                f a Nothing             = a
                f a (Just (o, Nothing)) = App o a
                f a (Just (o, Just b))  = App (App o a) b

        infixfp :: OpeParser
        infixfp p = f <$> g
            where
                f :: [Expr] -> Expr
                f [a]       = a
                f [a, o]    = App o a
                f [a, o, b] = App (App o a) b
                f (a:o:b:x) = App (App (Const "and") $ f [a, o, b]) $ f (b:x)
                g :: Parser [Expr]
                g = (:) <$> p <*> (concat <$> optionL ((:) <$> oper ifs <*> g))

        oper s = Const <$> oneof sign s <* notp symbol

-- expr' :: Parser Expr
-- -- expr = construct <|> term <* eof
-- -- expr = const (Const "A") <$> string "abc"
-- expr' = term <* many (sat isSpace) <* eof

-- construct :: Parser Expr
-- construct = Cons <$> atom <*> many term
--
-- atom :: Parser String
-- atom = token $ (:) <$> upper <*> many (letter <|> digit)

bind :: OpeParser
bind p = (,) <$> p <*> option (sign "=" *> p) >>= f
    where
        f (a, Nothing) = return a
        f (a, Just b)  = case appToList a of
            [Const s]   -> return $ Bind s b
            [Data s _]  -> return $ Bind s b
            Const s:es  -> return $ Bind s $ foldr1 Lambda $ es ++ [b]
            Data s _:es -> return $ Bind s $ foldr1 Lambda $ es ++ [b]
            _           -> mzero

lambda :: Parser Expr -> OpeParser
lambda p q = (f <$> (sign "\\" *> some p) <* sign "->" <*> q)
             <|> q
    where
        f es e = foldr1 Lambda $ es ++ [e]

declaration :: OpeParser
declaration p = f <$> p <*> option (sign "!" *> ((:) <$> declaration p <*> many (sign ";" *> declaration p)))
    where
        f a Nothing   = a
        f a (Just ds) = Decl ds a

apply :: [String] -> OpeParser
apply ss p = foldl App <$> termop ss p <*> many (term ss p)

termop :: [String] -> OpeParser
termop ss p = term ss p <|> operator ss

term :: [String] -> OpeParser
term ss p = sign "(" *> p <* sign ")"
        <|> list p
        <|> variable
        <|> (datacons >>= f)
        <|> (constant >>= f)
        <|> float
        <|> number
        <|> systemConst
        <|> Const <$> sign "_"
        <|> const (Comma []) <$> sign "()"
    where
        f (Const s) | s `elem` ss = mzero
        f a = return a

operator :: [String] -> Parser Expr
operator ss = Const <$> oneof sign ss

list :: OpeParser
list p = (makeList .) . (++) <$> (sign "[" *> optionL p) <*> many (sign "," *> p) <* sign "]"
    where
        makeList :: [Expr] -> Expr
        -- makeList = foldr (\a b -> App (App (Data ":" []) a) b) (Data "[]" [])
        makeList = foldr (\a b -> Data ":" [a, b]) (Data "[]" [])

variable :: Parser Expr
variable = Var 0 <$> token ((:) <$> char '_' <*> some (letter <|> digit))

constant :: Parser Expr
constant = Const <$> token (((:) <$> letter <*> many (letter <|> digit))
                            <|> some symbol)

datacons :: Parser Expr
datacons = (`Data` [])  <$> token ((:) <$> upper <*> many (letter <|> digit))

systemConst :: Parser Expr
systemConst = (`Data` []) <$> token ((:) <$> char '#' <*> many letter)

float :: Parser Expr
float = toFraction <$> (spaces *> some digit) <* char '.' <*> some digit
    where
        toFraction xs ys = App (App (Data "/" []) (Data (xs ++ ys) [])) (Data (show $ 10 ^ length ys) [])
        -- toFraction xs ys = Cons "/" [int . read $ xs ++ ys, int $ 10 ^ length ys]

number :: Parser Expr
number = (`Data` []) <$> (spaces *> some digit)

-- int :: Integer -> Expr
-- int i = Cons "_Int" [Bytes i]

sign :: String -> Parser String
sign s = token $ string s

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isLetter

upper :: Parser Char
upper = sat isUpper

symbol :: Parser Char
symbol = oneof char "!$%&*+./<=>?@\\^|-~:"

spaces :: Parser String
spaces = many space

space :: Parser Char
space = sat isSeparator

string :: String -> Parser String
string ""       = return ""
string (c : "") = (:[]) <$> char c
string (c : cs) = (:) <$> char c <*> string cs

char :: Char -> Parser Char
char c = sat (c ==)

option :: Parser a -> Parser (Maybe a)
option p = (Just <$> p) <|> return Nothing

optionL :: Parser a -> Parser [a]
optionL p = maybeToList <$> option p

oneof :: (a -> Parser b) -> [a] -> Parser b
oneof p = foldl (\q a -> q <|> p a) mzero


-- or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- f `or` g = \a -> f a || g a
