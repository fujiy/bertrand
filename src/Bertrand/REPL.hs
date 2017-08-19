
module Bertrand.REPL
    ( repl
    ) where

import System.IO
import Control.Monad
import Control.Monad.Extra
-- import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

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
        ("clear",   const $ do
            put mempty
            outputStrLn "cleared all declarations"),
        ("options", const $ do
            REPLST _ ops <- get
            outputStr $ unlines . map show $ ops),
        ("binds", \ss -> do
            REPLST env _ <- get
            outputStr $ if null ss
            then unlines . map show . M.toList $ binds env
            else unlines . map show . concat . M.lookup (head ss) $ binds env),
        ("cstrs", \ss -> do
            REPLST env _ <- get
            outputStr $ if null ss
            then unlines . map show . M.toList $ cstrs env
            else unlines . map show . concat . M.lookup (head ss) $ cstrs env),
        ("decls", const $ do
            REPLST env _ <- get
            outputStr $ unlines . map show $ decls env),
        ("variables", const $ do
            REPLST env _ <- get
            outputStrLn $ "var: "  ++ (unwords . map show . fst $ vars env)
            outputStrLn $ "cons: " ++ (unwords . map show . snd $ vars env) )
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
    '?' -> case preprocess (init xs) of
        ("", _) -> return ()
        (s,  _) -> do
            m <- parseS $ "it = ternary (" ++ init s ++ ")"
            whenJust m $ \e -> do
                let a = head . fromJust . M.lookup "it" $ binds e
                REPLST env _ <- get
                outputStrLn . evalShow .
                    Env (fst preludeM){depth = -2} $ Env env{depth = -1} a
    _   -> case preprocess xs of
        ("", _) -> return ()
        (s,  _) -> do
            m <- parseS $ "it = " ++ init s
            whenJust m $ \e -> do
                let a = head . fromJust . M.lookup "it" $ binds e
                REPLST env _ <- get
                outputStrLn $ show a
                outputStrLn . evalShow .
                    Env (fst preludeM){depth = -2} $ Env env{depth = -1} a

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
    \   <expr>          evaluate and display <expr>\n\
    \   <expr> ?        display <expr> is true, false or undefined\n\
    \   <statement> .   declare that <statement> is true\n\
    \   :binds [<name>] display bindings of <name>\n\
    \   :clear          clear all declaretions\n\
    \   :cstrs [<name>] display constraints of <name>\n\
    \   :decls          display all declarations\n\
    \   :options        display all parse options\n\
    \\n\
    \   :help, :?       display this list of commands\n\
    \   :quit           exit Bertrand Interpreter"
