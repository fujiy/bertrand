
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
evalF xs = let c = last xs in
           if c == '.'
           then case preprocess (init xs) of
               ("", op) -> modify (`mappend` REPLST [] op)
               (s,  _)  -> do
                   m <- parseS $ init s
                   maybe (return ())
                         (\e -> modify (`mappend` REPLST [e] [])) m
           else case preprocess xs of
               ("", _) -> return ()
               (s,  _) -> do
                   m <- parseS $ init s
                   REPLST es _ <- get
                   maybe (return ())
                         (outputStrLn . evalShow . Decl (fst prelude) . Decl es) m

-- repl :: IO ()
-- repl = do
--     putStrLn "Hello!  Bertrand, ver.0.1   :? for help"
--
--     runStateT roop mempty
--
--     putStrLn "See you!"

-- type REPL a = StateT REPLST IO a

-- io :: IO a -> REPL a
-- io = liftIO
--
-- roop :: REPL ()
-- roop = do
--     io $ putStr "main> "
--     io $ hFlush stdout
--     input <- io getInput
--
--     case input of
--         ':':xs -> command xs
--         _ | last input == '.' -> do
--              case preprocess (init input) of
--                  ("", op) -> modify (`mappend` REPLST [] op)
--                  (s,  _)  -> do
--                      m <- parseS $ init s
--                      maybe (return ()) (\e -> modify (`mappend` REPLST [e] [])) m
--              roop
--           | otherwise -> do
--               case preprocess input of
--                   ("", _) -> return ()
--                   (s,  _) -> do
--                       m <- parseS $ init s
--                       REPLST es _ <- get
--                       maybe (return ()) (io . putStrLn . evalShow . Decl (fst prelude) . Decl es) m
--               roop


    -- if head input == ':'
    --     then command $ tail input
    --     else do
    --         case last input of
    --             '.' -> declare $ init input
    --             '?' -> bool $ init input
    --             _   -> eval input


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
            "data ()",
            "data []",
            "data Id",
            "true",
            "id x = x"
            ]

--------------------------------------------------------------------------------

helptext :: String
helptext = unlines [
    " Commands available from the prompt:",
    "",
    "   <expr>         evaluate and display <expr>",
    "   <expr> .       declare that <expr> is true",
    "   <expr> ?       display <expr> is true, false or undefined",
    "   :clear         clear all declarations",
    "   :help, :?      display this list of commands",
    -- "   :i             display all declarations",
    -- "   :i <expr>      display all declarations about <expr>",
    "   :options       display all parse options",
    "   :quit          exit Bertrand Interpreter"
    ]
