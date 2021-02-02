{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Pty
-- Copyright   :  (C) 2013 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  haha
--
-- A module for interacting with subprocesses through a pseudo terminal (pty).
-- Provides functions for reading from, writing to and resizing pseudo
-- terminals. Re-exports most of "System.Posix.Terminal", providing wrappers
-- that work with the 'Pty' type where necessary.
-------------------------------------------------------------------------------
module System.Posix.Pty (
    -- * Subprocess Creation
      spawnWithPty
    , spawnWithPtyPixel
    -- * Data Structures
    , Pty
    , PtyControlCode (..)
    -- * Pty Interaction Functions
    , createPty
    , closePty
    , tryReadPty
    , readPty
    , writePty
    , resizePty
    , resizePtyPixel
    , ptyDimensions
    , ptyDimensionsPixel
    -- * Blocking on 'Pty's
    , threadWaitReadPty
    , threadWaitWritePty
    , threadWaitReadPtySTM
    , threadWaitWritePtySTM
    -- * Re-exports of "System.Posix.Terminal"
    -- $posix-reexport
    , getTerminalAttributes
    , setTerminalAttributes
    , sendBreak
    , drainOutput
    , discardData
    , controlFlow
    , getTerminalProcessGroupID
    , getTerminalName
    , getSlaveTerminalName
    , module System.Posix.Terminal
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Concurrent (withMVar)
import Control.Exception (bracket, throwIO, ErrorCall(..))
import Control.Monad (when)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (createAndTrim)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

import GHC.Conc (STM)
import GHC.Conc.IO (threadWaitRead, threadWaitWrite,
                    threadWaitReadSTM, threadWaitWriteSTM)

import Foreign
import Foreign.C.Error (throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types

import System.IO.Error (mkIOError, eofErrorType)
import System.Posix.IO (fdReadBuf, fdWriteBuf,closeFd)
import System.Posix.Types
import System.Process.Internals (mkProcessHandle, runInteractiveProcess_lock, ProcessHandle)

import qualified System.Posix.Terminal as T
import System.Posix.Terminal hiding
        ( getTerminalAttributes
        , setTerminalAttributes
        , sendBreak
        , drainOutput
        , discardData
        , controlFlow
        , getTerminalProcessGroupID
        , setTerminalProcessGroupID
        , queryTerminal
        , getTerminalName
        , openPseudoTerminal
        , getSlaveTerminalName)

-- | Abstract pseudo terminal type.
newtype Pty = Pty Fd

-- | Pseudo terminal control information.
--
-- [Terminal read queue] The terminal read queue contains the data that was
-- written from the master terminal to the slave terminal, which was not read
-- from the slave yet.
--
-- [Terminal write queue] The terminal write queue contains the data that was
-- written from the slave terminal, which was not sent to the master yet.
data PtyControlCode = FlushRead     -- ^ Terminal read queue was flushed.
                    | FlushWrite    -- ^ Terminal write queue was flushed.
                    | OutputStopped -- ^ Terminal output was stopped.
                    | OutputStarted -- ^ Terminal output was restarted.
                    | DoStop        -- ^ Terminal stop and start characters are
                                    --   @^S@ and @^Q@ respectively.
                    | NoStop        -- ^ Terminal stop and start characters are
                                    --   NOT @^S@ and @^Q@.
                    deriving (Eq, Read, Show)

-- | Produces a 'Pty' if the file descriptor is associated with a terminal and
-- Nothing if not.
createPty :: Fd -> IO (Maybe Pty)
createPty fd = do
    isTerminal <- T.queryTerminal fd
    let result | isTerminal = Just (Pty fd)
               | otherwise  = Nothing
    return result

-- | Close this pseudo terminal.
closePty :: Pty -> IO ()
closePty (Pty fd) = closeFd fd

-- | Attempt to read data from a pseudo terminal. Produces either the data read
-- or a list of 'PtyControlCode'@s@ indicating which control status events that
-- have happened on the slave terminal.
--
-- Throws an 'IOError' of type 'eofErrorType' when the terminal has been
-- closed, for example when the subprocess has terminated.
tryReadPty :: Pty -> IO (Either [PtyControlCode] ByteString)
tryReadPty (Pty fd) = do
    result <- readBS 1024
    case BS.uncons result of
         Nothing -> ioError ptyClosed
         Just (byte, rest)
            | byte == 0    -> return (Right rest)
            | BS.null rest -> return $ Left (byteToControlCode byte)
            | otherwise    -> ioError can'tHappen
  where
    ptyClosed :: IOError
    ptyClosed = mkIOError eofErrorType "pty terminated" Nothing Nothing

    can'tHappen :: IOError
    can'tHappen = userError "Uh-oh! Something different went horribly wrong!"

    readBS :: ByteCount -> IO ByteString
    readBS n
      | n <= 0    = return BS.empty
      | overflow  = throwIO (ErrorCall "invalid size for read")
      | otherwise = BS.createAndTrim (fromIntegral n) $
                        fmap fromIntegral . fillBuf
      where
        overflow :: Bool
        overflow = n >= fromIntegral (maxBound :: Int)

        fillBuf :: Ptr Word8 -> IO ByteCount
        fillBuf buf = throwErrnoIfMinus1Retry "read failed" $
                            fdReadBuf fd buf n

-- | The same as 'tryReadPty', but discards any control status events.
readPty :: Pty -> IO ByteString
readPty pty = tryReadPty pty >>= \case
                   Left _ -> readPty pty
                   Right bs -> return bs

-- | Write a 'ByteString' to the pseudo terminal, throws an 'IOError' when the
-- terminal has been closed, for example when the subprocess has terminated.
writePty :: Pty -> ByteString -> IO ()
writePty (Pty fd) bs =
    BS.unsafeUseAsCString bs $ write (fromIntegral (BS.length bs)) . castPtr
  where
    write :: ByteCount -> Ptr Word8 -> IO ()
    write len buf = do
        res <- throwErrnoIfMinus1Retry "write failed" $ fdWriteBuf fd buf len
        when (res < len) $ do
            write (len - res) $ plusPtr buf (fromIntegral res)

-- | Set the pseudo terminal's dimensions to the specified width and height.
resizePty :: Pty -> (Int, Int) -> IO ()
resizePty pty (x, y) = resizePtyPixel pty (x, y, 0, 0)

-- | The same as 'resizePty', but also allows setting the dimension in pixels.
resizePtyPixel :: Pty -> (Int, Int, Int, Int) -> IO ()
resizePtyPixel (Pty fd) (x, y, xpixel, ypixel) =
  throwErrnoIfMinus1Retry_ "unable to set pty dimensions" $ set_pty_size fd x y xpixel ypixel

-- | Produces the pseudo terminal's current dimensions.
ptyDimensions :: Pty -> IO (Int, Int)
ptyDimensions = fmap (\(x, y, _, _) -> (x, y)) . ptyDimensionsPixel

-- | The same as 'ptyDimensions', but also returns the dimension in pixels (pixel dimensions might be zero when not supported by the terminal emulator).
ptyDimensionsPixel :: Pty -> IO (Int, Int, Int, Int)
ptyDimensionsPixel (Pty fd) = alloca $ \x -> alloca $ \y -> alloca $ \xpixel -> alloca $ \ypixel -> do
    throwErrnoIfMinus1Retry_ "unable to get pty size" $ get_pty_size fd x y xpixel ypixel
    (,,,) <$> peek x <*> peek y <*> peek xpixel <*> peek ypixel

-- | Create a new process that is connected to the current process through a
-- pseudo terminal. If an environment is specified, then only the specified
-- environment variables will be set. If no environment is specified the
-- process will inherit its environment from the current process. Example:
--
-- > pty <- spawnWithPty (Just [("SHELL", "tcsh")]) True "ls" ["-l"] (20, 10)
--
-- This searches the user's PATH for a binary called @ls@, then runs this
-- binary with the commandline argument @-l@ in a terminal that is 20
-- characters wide and 10 characters high. The environment of @ls@ will
-- contains one variable, SHELL, which will be set to the value \"tcsh\".
spawnWithPty :: Maybe [(String, String)]    -- ^ Optional environment for the
                                            --   new process.
             -> Bool                        -- ^ Search for the executable in
                                            --   PATH?
             -> FilePath                    -- ^ Program's name.
             -> [String]                    -- ^ Command line arguments for the
                                            --   program.
             -> (Int, Int)                  -- ^ Initial dimensions for the
                                            --   pseudo terminal.
             -> IO (Pty, ProcessHandle)
spawnWithPty env' search path' argv' (x, y) = spawnWithPtyPixel env' search path' argv' (x, y, 0, 0)

-- | Like 'spawnWithPty', but can also set pty dimensions in pixels.
spawnWithPtyPixel :: Maybe [(String, String)] -> Bool -> FilePath -> [String] -> (Int, Int, Int, Int) -> IO (Pty, ProcessHandle)
spawnWithPtyPixel env' (fromBool -> search) path' argv' (x, y, xpixel, ypixel) = do
    bracket allocStrings cleanupStrings $ \(path, argvList, envList) -> do
        let allocLists = do
                argv <- newArray0 nullPtr (path : argvList)
                env <- case envList of
                        [] -> return nullPtr
                        _ -> newArray0 nullPtr envList
                return (argv, env)

            cleanupLists (argv, env) = free argv >> free env

        bracket allocLists cleanupLists $ \(argv, env) -> do
            alloca $ \pidPtr -> do
                fd <- throwErrnoIfMinus1Retry "failed to fork or open pty" $
                        withMVar runInteractiveProcess_lock $ \_ ->
                          fork_exec_with_pty x y xpixel ypixel search path argv env pidPtr

                pid <- peek pidPtr
                handle <- mkProcessHandle (fromIntegral pid) True

                return (Pty fd, handle)
  where
    fuse :: (String, String) -> IO CString
    fuse (key, val) = newCString (key ++ "=" ++ val)

    allocStrings :: IO (CString, [CString], [CString])
    allocStrings = do
        path <- newCString path'
        argv <- mapM newCString argv'
        env <- maybe (return []) (mapM fuse) env'
        return (path, argv, env)

    cleanupStrings :: (CString, [CString], [CString]) -> IO ()
    cleanupStrings (path, argv, env) = do
        free path
        mapM_ free argv
        mapM_ free env

-- Module internal functions

getFd :: Pty -> Fd
getFd (Pty fd) = fd

byteToControlCode :: Word8 -> [PtyControlCode]
byteToControlCode i = map snd $ filter ((/=0) . (.&.i) . fst) codeMapping
    where codeMapping :: [(Word8, PtyControlCode)]
          codeMapping =
            [ (tiocPktFlushRead,  FlushRead)
            , (tiocPktFlushWrite, FlushWrite)
            , (tiocPktStop,       OutputStopped)
            , (tiocPktStart,      OutputStarted)
            , (tiocPktDoStop,     DoStop)
            , (tiocPktNoStop,     NoStop)
            ]

-- Foreign imports

foreign import capi unsafe "sys/ioctl.h value TIOCPKT_FLUSHREAD"
    tiocPktFlushRead :: Word8
foreign import capi unsafe "sys/ioctl.h value TIOCPKT_FLUSHWRITE"
    tiocPktFlushWrite :: Word8
foreign import capi unsafe "sys/ioctl.h value TIOCPKT_STOP"
    tiocPktStop :: Word8
foreign import capi unsafe "sys/ioctl.h value TIOCPKT_START"
    tiocPktStart :: Word8
foreign import capi unsafe "sys/ioctl.h value TIOCPKT_DOSTOP"
    tiocPktDoStop :: Word8
foreign import capi unsafe "sys/ioctl.h value TIOCPKT_NOSTOP"
    tiocPktNoStop :: Word8

foreign import ccall "pty_size.h"
    set_pty_size :: Fd -> Int -> Int -> Int -> Int -> IO CInt

foreign import ccall "pty_size.h"
    get_pty_size :: Fd -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO CInt

foreign import ccall "fork_exec_with_pty.h"
    fork_exec_with_pty :: Int
                       -> Int
                       -> Int
                       -> Int
                       -> CInt
                       -> CString
                       -> Ptr CString
                       -> Ptr CString
                       -> Ptr Int
                       -> IO Fd

-- Pty specialised versions of GHC.Conc.IO
-- | Equivalent to 'threadWaitRead'.
threadWaitReadPty :: Pty -> IO ()
threadWaitReadPty = threadWaitRead . getFd

-- | Equivalent to 'threadWaitWrite'.
threadWaitWritePty :: Pty -> IO ()
threadWaitWritePty = threadWaitWrite . getFd

-- | Equivalent to 'threadWaitReadSTM'.
threadWaitReadPtySTM :: Pty -> IO (STM (), IO ())
threadWaitReadPtySTM = threadWaitReadSTM . getFd

-- | Equivalent to 'threadWaitWriteSTM'.
threadWaitWritePtySTM :: Pty -> IO (STM (), IO ())
threadWaitWritePtySTM = threadWaitWriteSTM . getFd

-- Pty specialised re-exports of System.Posix.Terminal

{- $posix-reexport
This module re-exports the entirety of "System.Posix.Terminal", with the
exception of the following functions:

[setTerminalProcessGroupID] This function can't be used after a process using
the slave terminal has been created, rendering it mostly useless for working
with 'Pty'@s@ created by this module.

[queryTerminal] Useless, 'Pty' is always a terminal.

[openPseudoTerminal] Only useful for the kind of tasks this module is supposed
abstract away.

In addition, some functions from "System.Posix.Terminal" work directly with
'Fd'@s@, these have been hidden and instead the following replacements working
on 'Pty'@s@ are exported.
-}

-- | See 'System.Posix.Terminal.getTerminalAttributes'.
getTerminalAttributes :: Pty -> IO TerminalAttributes
getTerminalAttributes = T.getTerminalAttributes . getFd

-- | See 'System.Posix.Terminal.setTerminalAttributes'.
setTerminalAttributes :: Pty -> TerminalAttributes -> TerminalState -> IO ()
setTerminalAttributes = T.setTerminalAttributes . getFd

-- | See 'System.Posix.Terminal.sendBreak'.
sendBreak :: Pty -> Int -> IO ()
sendBreak = T.sendBreak . getFd

-- | See 'System.Posix.Terminal.drainOutput'.
drainOutput :: Pty -> IO ()
drainOutput = T.drainOutput . getFd

-- | See 'System.Posix.Terminal.discardData'.
discardData :: Pty -> QueueSelector -> IO ()
discardData = T.discardData . getFd

-- | See 'System.Posix.Terminal.controlFlow'.
controlFlow :: Pty -> FlowAction -> IO ()
controlFlow = T.controlFlow . getFd

-- | See 'System.Posix.Terminal.getTerminalProcessGroupID'.
getTerminalProcessGroupID :: Pty -> IO ProcessGroupID
getTerminalProcessGroupID = T.getTerminalProcessGroupID . getFd

-- | See 'System.Posix.Terminal.getTerminalName'.
getTerminalName :: Pty -> IO FilePath
getTerminalName = T.getTerminalName . getFd

-- | See 'System.Posix.Terminal.getSlaveTerminalName'.
getSlaveTerminalName :: Pty -> IO FilePath
getSlaveTerminalName = T.getSlaveTerminalName . getFd
