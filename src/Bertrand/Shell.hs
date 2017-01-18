
module Bertrand.Shell
    (Shell,
     ShellDesc(..),
     ShellStyle(..),
    --  ShellCommand,
     CommandFunc,
     shell,
     shellDesc,
     shellStyle,
     get,
     put,
     modify,
     outputStr,
     outputStrLn
    ) where

import Prelude hiding (putChar, putStr, putStrLn)
import qualified System.IO as IO
import Control.Monad
import qualified Control.Monad.State as ST
import qualified Control.Monad.Trans as MT
import Data.Char
import Data.List
import System.Console.Haskeline hiding (outputStr, outputStrLn)

import Debug.Trace


type Shell s = ST.StateT (ShellDesc s, s) (InputT IO)

runShell :: Shell s a -> ShellDesc s -> s -> IO (a, (ShellDesc s, s))
runShell sh sd s = runInputT defaultSettings $ ST.runStateT sh (sd, s)


-- data ShellST = ShellST [String]
-- shellST      = ShellST []

data ShellDesc s = ShellDesc {commands :: [(String, CommandFunc s)],
                              evalFunc :: String -> Shell s (),
                              prompt   :: s -> String,
                              style    :: ShellStyle }
shellDesc        = ShellDesc {commands = [],
                              evalFunc = outputStrLn,
                              prompt   = const "> ",
                              style    = shellStyle }

data ShellStyle = ShellStyle {startText     :: String,
                              quitText      :: String,
                              commandPrefix :: Char }
shellStyle      = ShellStyle {startText     = "",
                              quitText      = "",
                              commandPrefix = ':' }

type Document = String

type CommandFunc s = [String] -> Shell s ()

--------------------------------------------------------------------------------

shell :: ShellDesc s -> s -> IO ()
shell sd s = void $ runShell sh sd s
    where
        sh :: Shell s ()
        sh = do
            io $ IO.hSetBuffering IO.stdin IO.NoBuffering
            io $ IO.hSetEcho IO.stdin False

            (sd, s) <- ST.get
            outputStrLn $ startText $ style sd
            roop

        roop :: Shell s ()
        roop = do
            (sd, s) <- ST.get

            m <- lift $ getInputLine $ prompt sd s
            case m of
                Nothing -> return ()
                Just (c:cs) | c == commandPrefix (style sd)
                        -> case words cs of
                            [] -> do
                                maybe (return ())
                                      ($ tail [])
                                      (lookup "help" $ commands sd)
                                roop
                            "quit":_ -> outputStrLn $ quitText $ style sd
                            xs -> do
                                maybe (outputStrLn "unknown command")
                                      (\(_,f) -> f $ tail xs)
                                      (find (\(s,_) -> s == head xs) (commands sd))
                                roop
                Just cs -> do
                    evalFunc sd cs
                    roop


get :: Shell s s
get = do
    (_, s) <- ST.get
    return s

put :: s -> Shell s ()
put s = do
    (sd, _) <- ST.get
    ST.put (sd, s)

modify :: (s -> s) -> Shell s ()
modify f = do
    (sd, s) <- ST.get
    ST.put (sd, f s)

lift :: InputT IO a -> Shell s a
lift = ST.lift

io :: IO a -> Shell s a
io = MT.lift . ST.lift

outputChar :: Char -> Shell s ()
outputChar = io . IO.putChar

flush :: Shell s ()
flush = io $ IO.hFlush IO.stdout

outputStr :: String -> Shell s ()
outputStr = io . IO.putStr

outputStrLn :: String -> Shell s ()
outputStrLn = io . IO.putStrLn


insertAt :: Int -> a -> [a] -> [a]
insertAt i y xs = let (as,bs) = splitAt i xs
                  in as ++ (y:bs)

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = let (as,bs) = splitAt i xs
                in init as ++ bs


-- tmap :: (a -> b, c -> d) -> (a, c) -> (b, d)
-- tmap (f, g) (a, b) = (f a, g b)
