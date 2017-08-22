{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Bertrand.Parser
    (parse
    )where

import Prelude hiding (null)
import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.State
import Data.Char
import qualified Data.Map as M
import Data.Maybe

-- import Debug.Trace

import Bertrand.Data
import Bertrand.System (systemIds)


--------------------------------------------------------------------------------

-- type Parser a = MaybeT (State ParserState) a

-- runParser = runMaybeT

newtype Parser a = Parser {runParser :: ParserState -> (Maybe a, ParserState)}

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

parse :: [ParseOption] -> Int -> String -> Either (Int, Int) Envir
parse os i cs = let (m, ParserState p _) = runParser (parser os i) (purePS cs)
                in  maybe (Left p) Right m

--------------------------------------------------------------------------------

type OpeParser = Parser Expr -> Parser Expr

parser :: [ParseOption] -> Int -> Parser Envir
parser ops i = emap (env $ i + 1) <$> statement <* eof
    where
        env :: Int -> Expr -> Expr
        env i = \case
            App a b    -> App (env i a) (env i b)
            Lambda a b -> Lambda (env (i + 1) a) (env (i + 1) b)
            Env e a    -> Env (emap (env $ i + 1) e {depth = i}) $ env (i + 1) a
            a          -> a

        expr :: Parser Expr
        expr = foldr id apply opeparsers
        -- expr = apply $ head opeparsers $ term expr

        statement :: Parser Envir
        statement = variable
                <|> constraint
                -- <|> bind
                <|> declare

        variable :: Parser Envir
        variable = (\s ss -> mempty{vars = case s of
                       "var"  -> (ss, [])
                       "cons" -> ([], ss)}) <$>
                       oneof string ["var", "cons"] <*> some identifier'

        constraint :: Parser Envir
        constraint = (\ss e -> mempty{cstrs = M.fromList $ map (, [e]) ss,
                                      decls = [(ss, e)]}) <$>
                         some identifier' <* sign "." <*> expr

        -- bind :: Parser Envir
        -- bind = (,) <$> expr <*> (sign "=" *> expr) >>= f
        --     where
        --         f (a, e) = let x:es = toList a in case detach x of
        --             Id s -> return mempty{binds = M.singleton s [foldr Lambda e es]}
        --             _    -> mzero

        declare :: Parser Envir
        declare = expr >>= f
            where
                f e = case toList e of
                    [a, b, c] | isName "=" a ->
                        let x:es = toList b
                        in case detach x of
                            Id s -> return mempty{binds =
                                        M.singleton s [foldr Lambda c es]}
                            _    -> mzero
                    _ -> return mempty{decls = [([], e)]}

        opeparsers :: [OpeParser]
        opeparsers = envir : lambda : map opeparser opers

        opers = operators ops

        lambda :: OpeParser
        lambda p = (f <$> (sign "\\" *> some term) <* sign "->" <*> p)
                     <|> p
            where
                f es e = foldr1 Lambda $ es ++ [e]

        envir :: OpeParser
        envir p = f <$> p <*> option (sign "!" *>
                  ((:) <$> statement <*> many (sign ";" *> statement)))
            where
                f a Nothing   = a
                f a (Just es) = Env (mconcat es) a

        apply :: Parser Expr
        apply = foldl App <$> (term <|> operator) <*> many term

        -- termop :: OpeParser
        -- termop p = term <|> operator

        term :: Parser Expr
        term = sign "(" *> expr <* sign ")"
           <|> App (Id "~") <$> (sign "~" *> term)
           <|> list
           <|> ifelse
           <|> caseof
           <|> float
           <|> number
           <|> systemId
           <|> Id <$> (identifier <|> wildcard <|> sign "()")

        signs :: [String]
        signs = "!":";":"\\":"->":",":"if":"then":"else":"case":"of":
                foldr (\(is, ils, irs, ifs) s -> is ++ ils ++ irs ++ ifs ++ s) [] opers

        operator :: Parser Expr
        operator = Id <$> oneof sign signs

        identifier :: Parser String
        identifier = token ((:) <$> (letter <|> char '#') <*>
                                         many (letter <|> digit))
                     >>= \s -> if s `elem` signs
                               then mzero
                               else return s

        identifier' :: Parser String
        identifier' = token ((:) <$> (letter <|> char '#') <*>
                                         many (letter <|> digit))

        wildcard :: Parser String
        wildcard = token ((:) <$> char '_' <*> many (letter <|> digit))

        list :: Parser Expr
        list = (makeList .) . (++) <$>
               (sign "[" *> optionL expr) <*> many (sign "," *> expr) <* sign "]"

        ifelse :: Parser Expr
        ifelse = (\c a b -> App (App (App (Id "#if") c) a) b) <$>
                 (sign "if" *> expr) <*> (sign "then" *> expr) <*> (sign "else" *> expr)

        caseof :: Parser Expr
        caseof = (\e cs -> App (App (Id "comma") $ makeList cs) e) <$>
                 (sign "case" *> expr) <* sign "of" <*>
                 ((:) <$> clause <*> many (sign ";" *> clause))

            where
                clause :: Parser Expr
                clause = Lambda . foldr1 App <$> some term <* sign "->" <*> expr

        makeList :: [Expr] -> Expr
        makeList = foldr (app2 (Id ":")) (Id "[]")

operators :: [ParseOption] -> [([String], [String], [String], [String])]
operators = M.elems . foldr f M.empty
    where
        f :: ParseOption -> M.Map Int ([String], [String], [String], [String]) -> M.Map Int ([String], [String], [String], [String])
        f (Infix  i s) = M.insertWith (const $ map1st (s:)) i ([s], [], [], [])
        f (Infixl i s) = M.insertWith (const $ map2nd (s:)) i ([], [s], [], [])
        f (Infixr i s) = M.insertWith (const $ map3rd (s:)) i ([], [], [s], [])
        f (Infixf i s) = M.insertWith (const $ map4th (s:)) i ([], [], [], [s])

        map1st f (a, b, c, d) = (f a, b, c, d)
        map2nd f (a, b, c, d) = (a, f b, c, d)
        map3rd f (a, b, c, d) = (a, b, f c, d)
        map4th f (a, b, c, d) = (a, b, c, f d)

--            infix     infixl    infixr    infixf
opeparser :: ([String], [String], [String], [String]) -> OpeParser
opeparser (is, ils, irs, ifs) p = infixp $ infixlp $ infixrp $ infixfp p
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
                f (a:o:b:x) = App (App (Id "and") $ f [a, o, b]) $ f (b:x)
                g :: Parser [Expr]
                g = (:) <$> p <*> (concat <$> optionL ((:) <$> oper ifs <*> g))

        oper s = Id <$> oneof sign s <* notp symbol

systemId :: Parser Expr
systemId = token (char '#' *> many letter) >>=
               (\s -> maybe mzero return $ System <$> lookup s systemIds)


float :: Parser Expr
float = token (toFraction <$> some digit <* char '.' <*> some digit)
    where
        toFraction xs ys = app2 (Id "/") (System . Int . read $ xs ++ ys)
                                (System . Int $ 10 ^ length ys)

number :: Parser Expr
number = System . Int . read <$> token (some digit)

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


app2 :: Expr -> Expr -> Expr -> Expr
app2 a b = App (App a b)

-- or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- f `or` g = \a -> f a || g a
