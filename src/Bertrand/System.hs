{-# LANGUAGE LambdaCase #-}

module Bertrand.System
    (systemIds, prelude) where

import Bertrand.Data

systemIds :: [(String, SystemType)]
systemIds = map (\(Func s f) -> (s, Func s f)) [
    funcInt2Int "intadd" (+),
    funcInt2Int "intsub" (-),
    funcInt2Int "intmul" (*),
    funcInt2 "intcompare" intcompare]
    where
        funcInt2Int :: String -> (Integer -> Integer -> Integer) -> SystemType
        funcInt2Int s f = funcInt2 s (\x y -> System . Int $ f x y)

        funcInt2 :: String -> (Integer -> Integer -> Expr) -> SystemType
        funcInt2 s f =
            Func s $ \case
                Int x -> Just $ System $ Func (s ++ "'") $ \case
                    Int y -> Just $ f x y
                    _ -> Nothing
                _ -> Nothing

        intcompare :: Integer -> Integer -> Expr
        intcompare x y = case compare x y of
            LT -> Id "less"
            EQ -> Id "equal"
            GT -> Id "greater"

prelude :: String
prelude = unlines [
    "infixr 0 $",
    "infixf 2 =>",
    "infixr 3 or",
    "infixr 4 and",
    "infixf 5 ==",
    "infixf 5 /=",
    "infixf 5 <",
    "infixf 5 =<",
    "infixf 5 >",
    "infixf 5 >=",
    "infixr 6 :",
    "infixr 6 ++",
    "infixl 7 +",
    "infixl 7 -",
    "infixl 8 *",
    "infixl 8 /",
    "infixl 10 .",

    -- "(+) = #intadd",
    -- "(-) = #intsub",
    -- "(*) = #intmul",

    -- "#if true a _  = a",
    -- "#if false _ b = b",
    --

    "var true false",
    "true.true",
    "false.~false",
    
    "~~true",
    -- "cons and or",
    -- "true and true",
    -- "~(false and _)",
    -- "~(_ and false)",
    -- "true or _",
    -- "_ or true",
    -- "~(false or false)",

    -- "f true = 0",

    "ternary _true  = true",
    "ternary _false = false",
    "ternary _      = undefined",

    -- "comma (x:[]) = x",
    -- "comma (x:xs) = a ! a = x; a = comma xs",

    -- "_t and _t ! _t",
    -- "~ (_t and _f) ! _t; ~ _f",
    -- "~ (_f and _t) ! _t; ~ _f",
    -- "~ (_f and _f) ! _t; ~ _f",

    -- "id x = x",
    -- "const x _ = x",
    -- "f $ x = f x",

    -- "map f x = f x",
    -- "inc = #intadd 1",
    --
    -- "head (x:_) = x",
    -- "tail (_:xs) = xs",
    -- "last (x:[]) = x",
    -- "last (_:xs) = last xs",
    -- "init (x:_:[]) = [x]",
    -- "init (x:xs)   = x : init xs",
    -- "null [] = true",
    -- "null xs = false",
    -- "length []     = 0",
    -- "length (_:xs) = length xs + 1",
    -- "[]     ++ ys = ys",
    -- "(x:xs) ++ ys = x:(xs ++ ys)",
    -- "map _ []     = []",
    -- "map f (x:xs) = f x : map f xs",
    -- "foldl _ a []     = a",
    -- "foldl f a (x:xs) = foldl f (f a x) xs",
    -- "foldr f z []     = z",
    -- "foldr f z (x:xs) = f x (foldr f z xs)",
    -- "concat = foldr (++) []",
    -- "take _ []     = []",
    -- "take 0 _      = []",
    -- "take i (x:xs) = x : take (i - 1) xs",
    -- "drop _ []     = []",
    -- "drop 0 xs     = xs",
    -- "drop i (_:xs) = drop (i - 1) xs",

    -- "ls = [1,2,3,4,5]",

    -- "f x = a ! a = x",

    ""]
