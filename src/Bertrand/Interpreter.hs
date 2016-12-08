
module Bertrand.Interpreter
    (eval
    )where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.List
import qualified Data.Map as Map

import Bertrand.Data

import Debug.Trace


--------------------------------------------------------------------------------
data Envir = Envir Ref [Expr] Binds
           | Root
    deriving Show

type Binds = Map.Map String [Expr]


type Interpreter a b = a -> State (Memory Envir) b

interpret :: Interpreter a b -> Memory Envir -> a -> b
interpret f s a = let (b, s') = runState (f a) s
                  in traceShow s' b

eval :: Expr -> String
eval = let (m, ref) = singleton Root
       in show . interpret (envir ref) m

new :: Interpreter Envir Ref
new env = do
    m <- get
    let (m', ref) = newMemory env m
    put m'
    return ref

-- test :: Interpreter Expr Expr
-- -- test a = do
-- --     s <- get
-- --     traceShow s $ return a
-- test a = StateT $ \s -> traceShow s return (a, s)

envir :: Ref -> Interpreter Expr Expr
envir r (Decl [] a) = envir r a
envir r (Decl ds a) = do
    let (bs, es) = partition (\x -> case x of Bind _ _ -> True
                                              _        -> False) ds
        binds = foldr (\(Bind s a) -> Map.insertWith (++) s [a]) Map.empty bs
    ref <- new $ Envir r es binds
    m <- get
    Env ref `fmap` envir ref a
envir r a           = emapM (envir r) a



emapM :: Interpreter Expr Expr -> Interpreter Expr Expr
emapM f (Cons s as)  = Cons s `fmap` mapM f as
emapM f (App a b)    = do a' <- f a
                          b' <- f b
                          return $ App a' b'
emapM f (Lambda a b) = do a' <- f a
                          b' <- f b
                          return $ Lambda a' b'
emapM f (Bind s a)   = Bind s `fmap` f a
emapM f (Comma as)   = Comma `fmap` mapM f as
emapM f (Decl ds a)  = do ds' <- mapM f ds
                          a' <- f a
                          return $ Decl ds' a'
emapM f (Env r a)    = Env r `fmap` f a
emapM f a            = return a
