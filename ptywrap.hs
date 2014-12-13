{-# LANGUAGE LambdaCase #-}

import Control.Monad (forever, (>=>))
import qualified Data.ByteString.Char8 as C8

import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Exit (exitWith)
import System.Process (waitForProcess)
import System.Posix.Pty

main :: IO ()
main = do
  (command:args) <- getArgs >>= \case [] -> error "usage: ptywrap [command to run]"
                                      x -> return x
  hSetBuffering stdout NoBuffering
  (pty, proc) <- spawnWithPty Nothing False command args (80, 25)
  forever $ (tryIOError . readPty) pty >>= \
    case Left e | isEOFError e -> waitForProcess proc >>= exitWith
         Left e -> ioError e
         Right s -> C8.putStr s
