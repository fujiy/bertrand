
module Bertrand.Interpreter
    (eval
    )where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Map as Map

import Bertrand.Data



--------------------------------------------------------------------------------
data Envir = Envir Ref [Expr] (Map String Binds)
           | Root

data Binds = Binds

type Interpreter a b = a -> State (Memory Envir) b

interpret :: Interpreter a b -> Memory Envir -> a -> b
interpret f s a = evalState (f a) s

eval :: Expr -> String
eval = show . interpret envir memory

-- ((a -> b) -> c) -> (a -> (b, s)) -> (c, s)

-- ($$) :: ((a -> b) -> c) -> Interpreter a b -> Interpreter a c
-- f $$ g = StateT $ \s -> f (\a -> interpret g s a)


-- arg :: MonadTrans t => t ((->) a) a
-- arg = lift id

envir :: Interpreter Expr Expr
envir a = return $ f a
    where
        f (Decl a []) = f a
        f (Decl a es) = Decl a es
        f a           = emap f a
