
module Bertrand.Data
    ( Expr(..), emap
    , ParseOption(..)
    , Memory, Ref, memory, new, fromList
    ) where


import Data.Bits
import Data.List

--------------------------------------------------------------------------------
data ParseOption = Infix Int String
                 | Infixl Int String
                 | Infixr Int String
                 | Infixf Int String
                 | DataCons String

instance Show ParseOption where
    show (Infix i s)  = "infix "  ++ show i ++ " " ++ s
    show (Infixl i s) = "infixl " ++ show i ++ " " ++ s
    show (Infixr i s) = "infixr " ++ show i ++ " " ++ s
    show (Infixf i s) = "infixf " ++ show i ++ " " ++ s
    show (DataCons s)  = "data " ++ s

--------------------------------------------------------------------------------
data Expr = Var String
          | Bytes Integer
          | Cons String [Expr]
          | App Expr Expr
          | Lambda Expr Expr
          | Bind String Expr
          | Comma [Expr]
          | Decl Expr [Expr]
          | Env Expr Ref

instance Show Expr where
    show (Var s)      = s
    show (Bytes i)    = '#':show i
    show (Cons s es)  = "[" ++ s ++ " " ++ unwords (map show es) ++ "]"
    show (App a b)    = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda a b) = "(\\" ++ show a ++ " -> " ++ show b ++ ")"
    show (Bind a b)   = a ++ " = " ++ show b
    show (Comma as)   = "(" ++ intercalate ", " (map show as) ++ ")"
    show (Decl a as)  = "(" ++ show a ++ " ! " ++ intercalate "; " (map show as) ++ ")"
    show (Env a r)    = "*" ++ show a

emap :: (Expr -> Expr) -> Expr -> Expr
emap f (Cons s as)  = Cons s (map f as)
emap f (App a b)    = App (f a) (f b)
emap f (Lambda a b) = Lambda (f a) (f b)
emap f (Bind s a)   = Bind s (f a)
emap f (Comma as)   = Comma (map f as)
emap f (Decl a ds)  = Decl (f a) (map f ds)
emap f (Env a r)    = Env (f a) r
emap f a            = a


--------------------------------------------------------------------------------
data Memory a = Node Ref Int a (Memory a) (Memory a)
              | Leaf

instance Show a => Show (Memory a) where
    show Leaf = ""
    show (Node _ _ a l r) = "(" ++ show a ++ " " ++ show l ++ " " ++ show r ++ ")"

instance Functor Memory where
    _ `fmap` Leaf = Leaf
    f `fmap` Node i j a l r = Node i j (f a) (f `fmap` l) (f `fmap` r)


type Ref = Int

rootRef :: Ref
rootRef = shift (maxBound :: Int) (-1) + 1


memory :: Memory a
memory = Leaf

fromList :: [a] -> Memory a
fromList = foldr (\a m -> fst $ new a m) memory

new :: a -> Memory a -> (Memory a, Ref)
new = add rootRef
    where
        add :: Ref -> a -> Memory a -> (Memory a, Ref)
        add ref a Leaf = (Node ref (- shift ref (-1)) a Leaf Leaf, ref)
        add _ a (Node i j b l r) = if j < 0
            then let (m, ref) = add (i + j) a l
                 in (Node i (-j) b m r, ref)
            else let (m, ref) = add (i + j) a r
                 in (Node i (-j) b l m, ref)

-- type Memory a = M.IntMap a
-- type Ref = M.Key
--
-- newMemory :: Memory a -> a -> Memory a
-- newMemory m a = M.insert (M.size m) a m
--
-- readMemory :: Memory a -> Ref -> a
-- readMemory = (M.!)
--
-- writeMemory :: Memory a -> Ref -> a -> Memory a
-- writeMemory m r a = M.insert r a m
--
-- modifyMemory :: Memory a -> Ref -> (a -> a) -> Memory a
-- modifyMemory m r f = M.adjust f r m


--
--
-- data Token = NumL Integer
--            | CharL Char
--            | Name String
--     deriving (Eq, Show)
--
--
-- data Statement = Statement [Token]
--                | Fixity InfixType Int Token
--     deriving (Eq, Show)
--
-- data InfixType = Infix | InfixL | InfixR | InfixP
--     deriving (Eq, Show)
--
--
-- -- type Parser a = MaybeT (State ParserS) a
-- -- runParser = runState . runMaybeT
--
-- data Parser a = Parser {runParser :: ParserS -> (Maybe a, ParserS)}
--
-- instance Functor Parser where
--     fmap f p = Parser $ \s -> let (x, s') = runParser p s
--                               in  (fmap f x, s')
--
-- instance Applicative Parser where
--     pure x = Parser $ \s -> (pure x, s)
--     pf <*> q = Parser $ \s -> let (mf, s') = runParser pf s
--                               in maybe (mzero, s')
--                                        (\f -> let (x, s'') = runParser q s'
--                                               in maybe (mzero, s'')
--                                                        (\a -> (pure $ f a, s'')) x) mf
--
--                                     -- let (y, s'') = f s'
--                                     --     in (y <*> x, s'')
--
-- instance Monad Parser where
--     p >>= g = Parser $ \s -> let (x, s') = runParser p s
--                              in maybe (mzero, s)
--                                       (\a -> runParser (g a) s') x
--
-- instance Alternative Parser where
--     empty = Parser $ \s -> (empty, s)
--     p <|> q = Parser $ \s -> let (x, s1) = runParser p s
--                              in maybe (let (y, s2) = runParser q s
--                                        in maybe (empty, max s1 s2)
--                                                 (const (y, s2)) y)
--                                       (const (x, s1)) x
--
-- instance MonadPlus Parser
--
-- item :: Parser Char
-- item = Parser $ \s@(ParserS xs (i, j)) -> case xs of
--                     ""   -> (mzero, s)
--                     c:cs -> (pure c, ParserS cs (i, j + 1))
--
-- eof :: Parser ()
-- eof = Parser $ \s@(ParserS xs _) -> case xs of
--                     ""  -> (return (), s)
--                     _:_ -> (mzero, s)
--
--
-- -- eof = do
-- --     ParserS xs _ <- lift get
-- --     case xs of
-- --         ""    -> return ()
-- --         (_:_) -> mzero
-- --
-- -- test :: Parser Char
-- -- test = Parser $ \(ParserS xs _) -> case xs of
-- --                     ""  -> return '\0'
-- --                     _:_ -> empty
-- --
-- -- test = do
-- --     ParserS xs _ <- lift get
-- --     case xs of
-- --         ""    -> return '\0'
-- --         (y:_) -> return y
--
--
--
--
-- data ParserS = ParserS String (Int, Int)
--     deriving Show
--
-- instance Eq ParserS where
--     ParserS _ x == ParserS _ y = x == y
--
-- instance Ord ParserS where
--     ParserS _ (i, j) <= ParserS _ (m, n)
--         | i < m     = True
--         | i > m     = False
--         | otherwise = j <= n
--
-- purePS :: String -> ParserS
-- purePS xs = ParserS xs (1, 1)
--
--
--
-- data ParseError = ParseError ErrorType (Int, Int)
--     deriving (Eq, Show)
--
--
--
-- data ErrorType = UnusableChar
--     deriving (Eq, Show)
