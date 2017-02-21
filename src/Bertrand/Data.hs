{-# LANGUAGE ExistentialQuantification #-}

module Bertrand.Data
    ( Expr(..),  emap
    , Envir(..), eempty, fmapE, toList
    , Boolean(..)
    , Binds,     findBind
    , ParseOption(..)
    -- , Thunk(..), thunk
    -- , Memory, Ref, memory, singleton, fromList, newMemory, readMemory, writeMemory
    -- , notf, andf, orf
    ) where


import Control.Applicative hiding (Const)
import Control.Monad
-- import Data.Bits
import Data.List
import qualified Data.Map as Map

import Debug.Trace

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
    show (DataCons s) = "data " ++ s

--------------------------------------------------------------------------------
data Expr = Const String
          | Var Int String
          | Data String [Expr]
          | App Expr Expr
          | Lambda Expr Expr
          | Bind String Expr
          | Comma [Expr]
          | Env (Int, Envir) Expr
          | Decl [Expr] Expr
    deriving Eq

instance Show Expr where
    show (Const s)      = s
    show (Var i s)      = show i ++ s
    show (Data s [])    = '\'' : s
    show (Data s es)    = "('" ++ s ++ " " ++ unwords (map show es) ++ ")"
    show (App a b)      = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Lambda a b)   = "(\\" ++ show a ++ " -> " ++ show b ++ ")"
    show (Bind a b)     = a ++ " = " ++ show b
    show (Comma as)     = "(" ++ intercalate ", " (map show as) ++ ")"
    show (Decl ds a)    = "(" ++ show a ++ " !! " ++ intercalate "; " (map show ds) ++ ")"
    show (Env (i, e) a) = "(" ++ show a ++ " ! " ++ show i ++ show e ++ ")"
    -- show (Env (i, e) a) = show a

emap :: (Expr -> Expr) -> Expr -> Expr
emap f (Data s es)  = Data s (map f es)
emap f (App a b)    = App (f a) (f b)
emap f (Lambda a b) = Lambda (f a) (f b)
emap f (Bind s a)   = Bind s (f a)
emap f (Comma as)   = Comma (map f as)
emap f (Decl ds a)  = Decl (map f ds) (f a)
emap f (Env t a)    = Env t (emap f a)
emap f a            = a

--------------------------------------------------------------------------------
data Envir = Envir [Expr] Binds
    deriving Eq

instance Show Envir where
    show (Envir as bs) = show as
                        -- ++ show (Map.toList bs)

type Binds = Map.Map String [Expr]

eempty :: Envir
eempty = Envir [] mempty

findBind :: String -> Binds -> [Expr]
findBind = Map.findWithDefault []

-- toExprs :: Envir -> [Expr]
-- toExprs (Envir es _) = es

fmapE :: (Expr -> Expr) -> Envir -> Envir
fmapE f (Envir as bs) = Envir (fmap f as) (fmap (map f) bs)

toList :: Envir -> [Expr]
toList (Envir as bs) = as ++ concatMap snd (Map.toList bs)

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

-- notf :: (a -> Bool) -> a -> Bool
-- notf f = not . f

-- andf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- andf f g a = f a && g a

-- orf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
-- orf f g a = f a || g a
