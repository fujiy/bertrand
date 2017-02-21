
module Bertrand.REPL
    ( repl
    ) where

import System.IO
import Control.Monad
-- import Control.Monad.State
import Data.Either

import Bertrand.Shell
import Bertrand.Data
import Bertrand.Preprocessor
import Bertrand.Parser
import Bertrand.Interpreter

import Debug.Trace


data REPLST = REPLST [Expr] [ParseOption]

instance Monoid REPLST where
    mempty = REPLST [] []
    REPLST xs os `mappend` REPLST ys ps = REPLST (xs ++ ys) (os ++ ps)


sdesc  = shellDesc  {commands = cmds,
                     evalFunc = evalF,
                     prompt   = const "> ",
                     style    = sstyle }
sstyle = shellStyle {startText     = "Hello!  Bertrand, ver.0.1   :? for help",
                     quitText      = "See you!",
                     commandPrefix = ':' }

repl :: IO ()
repl = shell sdesc (REPLST [] [])

cmds :: [(String, CommandFunc REPLST)]
cmds = [("help",    const $ outputStrLn helptext),
        ("?",       const $ outputStrLn helptext),
        ("clear",   const $ do put mempty
                               outputStrLn "cleared all declarations"),
        ("options", const $ do REPLST _ ops <- get
                               outputStr $ unlines . map show $ ops)
        ]

evalF :: String -> Shell REPLST ()
evalF "" = return ()
evalF xs = case last xs of
    '.' -> case preprocess (init xs) of
        ("", op) -> modify (`mappend` REPLST [] op)
        (s,  _)  -> do
            m <- parseS $ init s
            maybe (return ())
                  (\e -> modify (`mappend` REPLST [e] [])) m
    '?' -> case preprocess (init xs) of
        ("", _) -> return ()
        (s,  _) -> do
            m <- parseS $ init s
            REPLST es _ <- get
            maybe (return ())
                  (outputStrLn . reasonShow . Decl (fst prelude) . Decl es) m
    _   -> case preprocess xs of
        ("", _) -> return ()
        (s,  _) -> do
            m <- parseS $ init s
            REPLST es _ <- get
            maybe (return ())
                  (outputStrLn . evalShow . Decl (fst prelude) . Decl es) m

parseS :: String -> Shell REPLST (Maybe Expr)
parseS s = do
    REPLST _ ops <- get
    either
        (\(i, j) -> do
            outputStrLn $ "main:" ++ show i ++ ":" ++ show j ++ " parse error"
            return Nothing)
        (return . Just) $ parse (snd prelude ++ ops) s



-- command :: String -> REPL ()
-- command s = case take 1 s of
--     "q" -> return ()
--     -- "i" -> do
--     --     info $ tail s
--     --     roop
--     "c" -> do
--         put mempty
--         io $ putStrLn "Cleared all declarations."
--         roop
--     "i" -> do
--         REPLST es _ <- get
--         io $ putStr $ unlines . map show $ es
--         roop
--     "o" -> do
--         REPLST _ ops <- get
--         io $ putStr $ unlines . map show $ ops
--         roop
--     "p" -> do
--         let (es, ops) = prelude
--         io $ putStrLn $ unlines . map show $ ops
--         io $ putStr $ unlines . map show $ es
--         roop
--     "?" -> do
--         io $ putStr helptext
--         roop
--
--     _ -> do
--         io $ putStrLn "command not find."
--         roop

-- getInput :: IO String
-- getInput = getLine

--------------------------------------------------------------------------------

prelude :: ([Expr], [ParseOption])
prelude = let (s, ops) = preprocess code
          in (rights $ map (parse ops) $ lines s, ops)
    where
        code = unlines [
            "infixr 0 $",
            "infixr 2 or",
            "infixr 3 and",
            "infixf 4 ==",
            "infixf 4 /=",
            "infixr 5 :",
            "infixr 5 ++",
            "infixl 6 +",
            "infixl 6 -",
            "infixl 7 *",
            "infixl 7 /",
            "infixl 9 .",

            "data /",
            "data :",
            "data []",
            "data Id",

            -- "(+) = #intplus",
            -- "(-) = #intminus",
            -- "(*) = #intmultiply",
            --
            "true",
            "~false",
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

            ""
            ]


--------------------------------------------------------------------------------

helptext :: String
helptext =
    " Commands available from the prompt:\n\
    \\n\
    \   <expr>         evaluate and display <expr>\n\
    \   <expr> .       declare that <expr> is true\n\
    \   <expr> ?       display <expr> is true, false or undefined\n\
    \   :clear         clear all declarations\n\
    \   :help, :?      display this list of commands\n\
    \   :options       display all parse options\n\
    \   :quit          exit Bertrand Interpreter"
