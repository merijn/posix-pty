{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forever, void)
import Data.ByteString as BS (putStr)
import System.Process (waitForProcess)
import System.Posix.Pty

main :: IO ()
main = do
    (pty, hnd) <- spawnWithPty Nothing True "stty" ["-a"] (10, 10)
    forever $ tryReadPty pty >>= \case
        Left e -> print e
        Right s -> BS.putStr s >> putStrLn ""
    void $ waitForProcess hnd
