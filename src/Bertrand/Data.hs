-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

module Bertrand.Data(
    Expr(..), toList, detach, detachEnv,
    SystemType(..),
    Envir(..), emap,
    Boolean(..),
    ParseOption(..)
    -- , Thunk(..), thunk
    -- , Memory, Ref, memory, singleton, fromList, newMemory, readMemory, writeMemory
    -- , notf, andf, orf
    ) where


import Control.Applicative
import Control.Monad
-- import Data.Bits
-- import Data.List
-- import qualified Data.Map as Map

import Debug.Trace

--------------------------------------------------------------------------------
data ParseOption = Infix Int String
                 | Infixl Int String
                 | Infixr Int String
                 | Infixf Int String
                --  | DataCons String

instance Show ParseOption where
    show (Infix i s)  = "infix "  ++ show i ++ " " ++ s
    show (Infixl i s) = "infixl " ++ show i ++ " " ++ s
    show (Infixr i s) = "infixr " ++ show i ++ " " ++ s
    show (Infixf i s) = "infixf " ++ show i ++ " " ++ s
    -- show (DataCons s) = "data " ++ s

--------------------------------------------------------------------------------
data Expr = Id String
          | App Expr Expr
          | Lambda Expr Expr
          | Env Envir Expr
          | System SystemType
    deriving Eq

data SystemType = Int Integer
                | Func String (SystemType -> Maybe Expr)

instance Show Expr where
    show = \case
        Id s       -> s
        App a b    -> "(" ++ show a ++ " " ++ show b ++ ")"
        Lambda a b -> "(\\" ++ show a ++ " -> " ++ show b ++ ")"
        Env e a     -> show a
        -- Env e a    -> "(" ++ show a ++ " ! " ++ show e ++ ")"
        System s   -> show s

instance Show SystemType where
    show = \case
        Int i    -> show i
        Func s f -> '#':s

instance Eq SystemType where
    Int x    == Int y     = x == y
    Func s _ == Func s' _ = s == s'
    _        == _         = False

toList :: Expr -> [Expr]
toList = \case
    Env e a -> Env e <$> toList a
    App a b -> toList a ++ [b]
    a       -> [a]

detach :: Expr -> Expr
detach = snd . detachEnv

detachEnv :: Expr -> (Expr -> Expr, Expr)
detachEnv = \case
    Env t a -> let (f, a') = detachEnv a
               in  (Env t . f, a')
    a       -> (id, a)

--------------------------------------------------------------------------------
data Envir = Envir { binds :: [(String, Expr)],
                     decls :: [Expr],
                     vars  :: [String],
                     depth :: Int }

instance Eq Envir where
    ex == ey = depth ex == depth ey

instance Ord Envir where
    ex <= ey = depth ex <= depth ey

instance Monoid Envir where
    mempty = Envir [] [] [] 0
    Envir bs ds vs i `mappend` Envir bs' ds' vs' _
        = Envir (bs ++ bs') (ds ++ ds') (vs ++ vs') i

instance Show Envir where
    show (Envir bs ds _ i) = show i ++ show bs

-- toExprs :: Envir -> [Expr]
-- toExprs (Envir es _) = es

emap :: (Expr -> Expr) -> Envir -> Envir
emap f (Envir bs ds vs i) = Envir (map (mapSnd f) bs) (map f ds) vs i
--
-- toList :: Envir -> [Expr]
-- toList (Envir as bs) = as ++ concatMap snd (Map.toList bs)

--------------------------------------------------------------------------------
data Boolean = T | F | U deriving Eq

instance Show Boolean where
    show T = "true"
    show F = "false"
    show U = "undefined"

--------------------------------------------------------------------------------
data Thunk f a = Thunk [f (Thunk f a)]
               | Pure a

instance Functor f => Functor (Thunk f) where
    f `fmap` Pure a   = Pure (f a)
    f `fmap` Thunk as = Thunk (map (fmap (fmap f)) as)

instance Functor f => Applicative (Thunk f) where
    pure  = return
    (<*>) = ap

instance Functor f => Monad (Thunk f) where
    return = Pure
    Pure a   >>= f = f a
    Thunk as >>= f = Thunk (map (fmap (>>= f)) as)

instance Applicative f => Alternative (Thunk f) where
    empty = Thunk []
    Pure a   <|> t        = Thunk [pure $ Pure a] <|> t
    t        <|> Pure a   = t <|> Thunk [pure $ Pure a]
    Thunk as <|> Thunk bs = Thunk $ as ++ bs

instance Applicative f => MonadPlus (Thunk f)

thunk :: f (Thunk f a) -> Thunk f a
thunk f = Thunk [f]
--
-- value :: Thunk a -> a
-- value (Pure a)    = a
-- value (Apply f b) = value f $ value b
--
-- step :: Thunk a -> Thunk a
-- step (Pure a) = Pure a
-- step (Apply (Pure f) b)    = f <$> b
-- step (Apply (Apply f a) b) =

-- instance Monad Lazy where
--     return = pure
--     Thunk a   >>= f = f a
--     Apply g b >>= f = Apply ((>>= f) . (g <*>)) b
    -- Free x >>= f = Free (fmap (>>= f) x)
    -- Pure x >>= f = f x

--------------------------------------------------------------------------------

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- notf :: (a -> Bool) -> a -> Bool
-- notf f = not . f

-- andf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- andf f g a = f a && g a

-- orf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- orf f g a = f a || g a
