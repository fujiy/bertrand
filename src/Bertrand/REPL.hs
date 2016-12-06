
module Bertrand.REPL
    ( repl
    ) where

import System.IO
import Control.Monad
import Control.Monad.State
import Data.Either

import Bertrand.Data
import Bertrand.Preprocessor
import Bertrand.Parser
import Bertrand.Interpreter

import Debug.Trace

repl :: IO ()
repl = do
    putStrLn "Hello!  Bertrand, ver.0.1   :? for help"

    runStateT roop mempty

    putStrLn "See you!"

type REPL a = StateT REPLST IO a

io :: IO a -> REPL a
io = liftIO

data REPLST = REPLST [Expr] [ParseOption]

instance Monoid REPLST where
    mempty = REPLST [] []
    REPLST xs os `mappend` REPLST ys ps = REPLST (xs ++ ys) (os ++ ps)

roop :: REPL ()
roop = do
    io $ putStr "main> "
    io $ hFlush stdout
    input <- io getInput

    case input of
        ':':xs -> command xs
        _ | last input == '.' -> do
             case preprocess (init input) of
                 ("", op) -> modify (`mappend` REPLST [] op)
                 (s,  _)  -> do
                     m <- parseS $ init s
                     maybe (return ()) (\e -> modify (`mappend` REPLST [e] [])) m
             roop
          | otherwise -> do
              case preprocess input of
                  ("", _) -> return ()
                  (s,  _) -> do
                      m <- parseS $ init s
                      REPLST es _ <- get
                      maybe (return ()) (\e -> io $ putStrLn $ eval (Decl e es)) m
              roop


    -- if head input == ':'
    --     then command $ tail input
    --     else do
    --         case last input of
    --             '.' -> declare $ init input
    --             '?' -> bool $ init input
    --             _   -> eval input


parseS :: String -> REPL (Maybe Expr)
parseS s = do
    REPLST _ ops <- get
    either
        (\(i, j) -> do
            io $ putStrLn $ "main:" ++ show i ++ ":" ++ show j ++ " parse error"
            return Nothing)
        (return . Just) $ parse (snd prelude ++ ops) s



command :: String -> REPL ()
command s = case take 1 s of
    "q" -> return ()
    -- "i" -> do
    --     info $ tail s
    --     roop
    "c" -> do
        put mempty
        io $ putStrLn "Cleared all declarations."
        roop
    "i" -> do
        REPLST es _ <- get
        io $ putStr $ unlines . map show $ es
        roop
    "o" -> do
        REPLST _ ops <- get
        io $ putStr $ unlines . map show $ ops
        roop
    "p" -> do
        let (es, ops) = prelude
        io $ putStrLn $ unlines . map show $ ops
        io $ putStr $ unlines . map show $ es
        roop
    "?" -> do
        io $ putStr helptext
        roop

    _ -> do
        io $ putStrLn "command not find."
        roop

getInput :: IO String
getInput = getLine

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
            "data ()",
            "data []",
            "true"
            ]

--------------------------------------------------------------------------------

helptext :: String
helptext = unlines [
    "Commands available from the prompt:",
    "",
    "  <expr>         evaluate and display <expr>",
    "  <expr> .       declare that <expr> is true",
    "  <expr> ?       display <expr> is true, false or undefined",
    "  :c             clear all declarations",
    "  :i             display all declarations",
    "  :i <expr>      display all declarations about <expr>",
    "  :o             display all parse options",
    "  :q             exit Bertrand Interpreter",
    "  :?             display this list of commands",
    ""
    ]
