{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as C8

import System.Posix.Pty

main :: IO ()
main = do
  (command:args) <- getArgs >>= \case [] -> error "usage: ptywrap [command to run]"
                                      x -> return x
  (pty, _) <- spawnWithPty Nothing False command args (80, 25)
  forever $ do
    r <- readPty pty
    C8.putStr r
