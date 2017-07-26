
module Bertrand.REPL
    ( repl
    ) where

import System.IO
import Control.Monad
-- import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.Monoid

import Bertrand.Shell
import Bertrand.Data
import Bertrand.System (prelude)
import Bertrand.Preprocessor
import Bertrand.Parser
import Bertrand.Interpreter

import Debug.Trace


data REPLST = REPLST Envir [ParseOption] deriving Show

instance Monoid REPLST where
    mempty = REPLST mempty []
    REPLST ex xs `mappend` REPLST ey ys = REPLST (ex <> ey) (xs ++ ys)

sdesc  = shellDesc  {commands = cmds,
                     evalFunc = evalF,
                     prompt   = const "> ",
                     style    = sstyle }
sstyle = shellStyle {startText     = "Hello!  Bertrand, ver.0.1   :? for help",
                     quitText      = "See you!",
                     commandPrefix = ':' }

repl :: IO ()
repl = shell sdesc (REPLST mempty [])

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
        ("", op) -> modify (`mappend` REPLST mempty op)
        (s,  _)  -> do
            m <- parseS $ init s
            maybe (return ())
                  (\e -> modify (`mappend` REPLST e [])) m
            s <- get
            traceShow s $ return ()
    -- '?' -> case preprocess (init xs) of
    --     ("", _) -> return ()
    --     (s,  _) -> do
    --         m <- parseS $ init s
    --         REPLST es _ <- get
    --         maybe (return ())
    --               (outputStrLn . reasonShow . Decl (fst prelude) . Decl es) m
    _   -> case preprocess xs of
        ("", _) -> return ()
        (s,  _) -> do
            m <- parseS $ init ("it = " ++ s)
            REPLST e _ <-  get
            outputStrLn $ show m
            maybe (return ())
                  (outputStrLn . evalShow .
                   Env (fst preludeM){depth = -2} . Env e{depth = -1} .
                   fromJust . lookup "it" . binds) m

parseS :: String -> Shell REPLST (Maybe Envir)
parseS s = do
    REPLST _ ops <- get
    either
        (\(i, j) -> do
            outputStrLn $ "main:" ++ show i ++ ":" ++ show j ++ " parse error"
            return Nothing)
        (return . Just) $ parse (snd preludeM ++ ops) s

preludeM :: (Envir, [ParseOption])
preludeM = traceShowId $
          let (s, ops) = preprocess prelude
          in (mconcat $ rights $ map (parse ops) $ lines s, ops)


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
