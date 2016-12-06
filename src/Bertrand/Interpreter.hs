
module Bertrand.Interpreter
    (eval
    )where

import Bertrand.Data


eval :: Expr -> String
eval = show . envir


envir :: Expr -> Expr
envir (Decl a []) = emap envir a
envir (Decl a es) = Decl a es
envir a = emap envir a
