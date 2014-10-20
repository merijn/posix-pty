{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
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
    -- * Data Structures
    , Pty
    , PtyControlCode (..)
    -- * Pty Interaction Functions
    , createPty
    , tryReadPty
    , readPty
    , writePty
    , resizePty
    , ptyDimensions
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

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Unsafe.Coerce (unsafeCoerce)
import Foreign
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types
import Foreign.C.Error (Errno(..), getErrno)

#if defined(linux_HOST_OS)
import Foreign.C.Error (eIO)
import System.IO.Error (catchIOError)
#endif

import System.IO (Handle)
import System.IO.Error (mkIOError, eofErrorType)
import System.Posix.IO.ByteString (fdToHandle)
import System.Posix.Types
import System.Process (ProcessHandle)
import System.Process.Internals (mkProcessHandle)

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
data Pty = Pty !Fd !Handle

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
    isTerm <- T.queryTerminal fd
    if isTerm
       then Just . Pty fd <$> fdToHandle fd
       else return Nothing

-- | Attempt to read data from a pseudo terminal. Produces either the data read
-- or a list of 'PtyControlCode'@s@ indicating which control status events that
-- have happened on the slave terminal.
--
-- Throws an 'IOError' of type 'eofErrorType' when the terminal has been
-- closed, for example when the subprocess has terminated.
tryReadPty :: Pty -> IO (Either [PtyControlCode] ByteString)
tryReadPty (Pty _ hnd) = do
    result <- wrap $ BS.hGetSome hnd 1024
    case BS.uncons result of
         Nothing -> ioError ptyClosed
         Just (byte, rest)
            | byte == 0    -> return (Right rest)
            | BS.null rest -> return $ Left (byteToControlCode byte)
            | otherwise    -> ioError can'tHappen
  where
    wrap :: IO a -> IO a
#if defined(linux_HOST_OS)
    -- Linux indicates slave pty EOF as EIO
    -- https://lkml.org/lkml/2009/4/8/578
    wrap action = catchIOError action $ \ioE -> do
      errno <- getErrno
      case errno of
          e | e == eIO -> ioError ptyClosed
          _ -> ioError ioE
#else
    wrap = id
#endif
    ptyClosed = mkIOError eofErrorType "pty terminated" Nothing Nothing
    can'tHappen = userError "Uh-oh! Something different went horribly wrong!"

-- | The same as 'tryReadPty', but discards any control status events.
readPty :: Pty -> IO ByteString
readPty pty = tryReadPty pty >>= \case
                   Left _ -> readPty pty
                   Right bs -> return bs

-- | Write a 'ByteString' to the pseudo terminal, throws an 'IOError' when the
-- terminal has been closed, for example when the subprocess has terminated.
writePty :: Pty -> ByteString -> IO ()
writePty (Pty _ hnd) = BS.hPut hnd

-- | Set the pseudo terminal's dimensions to the specified width and height.
resizePty :: Pty -> (Int, Int) -> IO ()
resizePty (Pty fd _) (x, y) =
    set_pty_size fd x y >>= throwCErrorOnMinus1 "unable to set pty dimensions"

-- | Produces the pseudo terminal's current dimensions.
ptyDimensions :: Pty -> IO (Int, Int)
ptyDimensions (Pty fd _) = alloca $ \x -> alloca $ \y -> do
    get_pty_size fd x y >>= throwCErrorOnMinus1 "unable to get pty size"
    (,) <$> peek x <*> peek y

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
spawnWithPty env' search path' argv' (x, y) = do
    path <- newCString path'
    argv <- mapM newCString argv'
    env <- maybe (return []) (mapM fuse) env'

    (ptyFd, cpid) <- forkExecWithPty x y path (fromBool search) argv env

    mapM_ free (env ++ argv)
    free path

    throwCErrorOnMinus1 "unable to fork or open new pty" ptyFd

    hnd <- fdToHandle ptyFd
    ph <- mkProcessHandle (unsafeCoerce cpid) False
    return (Pty ptyFd hnd, ph)
  where
    fuse (key, val) = newCString (key ++ "=" ++ val)

-- Module internal functions

getFd :: Pty -> Fd
getFd (Pty fd _) = fd

throwCErrorOnMinus1 :: (Eq a, Num a) => String -> a -> IO ()
throwCErrorOnMinus1 s i = when (i == -1) $ do
    errnoMsg <- getErrno >>= \(Errno code) -> (peekCString . strerror) code
    ioError . userError $ s ++ ": " ++ errnoMsg

forkExecWithPty :: Int
                -> Int
                -> CString
                -> CInt
                -> [CString]
                -> [CString]
                -> IO (Fd, CInt)
forkExecWithPty x y path search argv' env' = do
    argv <- newArray0 nullPtr (path:argv')
    env <- case env' of
                [] -> return nullPtr
                _  -> newArray0 nullPtr env'

    alloca $ \pid -> do
      result <- fork_exec_with_pty x y search path argv env pid
      free argv >> free env
      pid' <- peek pid
      return (result, pid')

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

tiocPktFlushRead :: Word8
tiocPktFlushRead = 1

tiocPktFlushWrite :: Word8
tiocPktFlushWrite = 2

tiocPktStop :: Word8
tiocPktStop = 4

tiocPktStart :: Word8
tiocPktStart = 8

tiocPktDoStop :: Word8
tiocPktDoStop = 32

tiocPktNoStop :: Word8
tiocPktNoStop = 16

foreign import ccall unsafe "string.h"
    strerror :: CInt -> CString

foreign import ccall "pty_size.h"
    set_pty_size :: Fd -> Int -> Int -> IO CInt

foreign import ccall "pty_size.h"
    get_pty_size :: Fd -> Ptr Int -> Ptr Int -> IO CInt

foreign import ccall "fork_exec_with_pty.h"
    fork_exec_with_pty :: Int
                       -> Int
                       -> CInt
                       -> CString
                       -> Ptr CString
                       -> Ptr CString
                       -> Ptr CInt
                       -> IO Fd

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
