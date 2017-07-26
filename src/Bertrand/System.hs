{-# LANGUAGE LambdaCase #-}

module Bertrand.System
    (systemIds, prelude) where

import Bertrand.Data

systemIds :: [(String, SystemType)]
systemIds = [
    ("intadd", func2Int "intadd" (+)),
    ("intsub", func2Int "intsub" (-)),
    ("intmul", func2Int "intmul" (*)) ]
    where
        func2Int :: String -> (Integer -> Integer -> Integer) -> SystemType
        func2Int s f =
            Func s $ \case
                Int x -> Just $ System $ Func (s ++ "'") $ \case
                    Int y -> Just $ System $ Int $ f x y
                    _ -> Nothing
                _ -> Nothing

prelude :: String
prelude = unlines [
    -- "infixr 0 $",
    -- "infixr 2 or",
    -- "infixr 3 and",
    -- "infixf 4 ==",
    -- "infixf 4 /=",
    -- "infixr 5 :",
    -- "infixr 5 ++",
    "infixl 6 +",
    "infixl 6 -",
    -- "infixl 7 *",
    -- "infixl 7 /",
    -- "infixl 9 .",

    "(+) = #intadd",
    "(-) = #intsub",
    -- "(*) = #intmul",

    -- "true",
    -- "~false",
    -- "~~_true",

    -- "_t and _t ! _t",
    -- "~ (_t and _f) ! _t; ~ _f",
    -- "~ (_f and _t) ! _t; ~ _f",
    -- "~ (_f and _f) ! _t; ~ _f",

    -- "id x = x",
    -- "const x _ = x",
    -- "f $ x = f x",
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
    -- "foldl f a []     = a",
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

    -- "f 0 = 1",
    -- "f 1 = f 0",
    -- "f 2 = f 1",
    -- "f 3 = f 2",
    -- "f 4 = f 3",
    -- "f 5 = f 4",
    -- "f 6 = f 5",
    -- "f 7 = f 6",
    -- "f 8 = f 7",
    -- "f 9 = f 8",
    -- "f 10 = f 9",
    -- "f 11 = f 10",
    -- "f 12 = f 11",
    -- "f 13 = f 12",
    -- "f 14 = f 13",
    -- "f x = f (#intsub x 1)",

    ""]
