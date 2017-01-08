{-# LANGUAGE TypeFamilies, CPP #-}
-- | Simple interface for shell scripting-like tasks.
module Control.Shell
  ( -- * Running Shell programs
    Shell, ExitReason (..)
  , shell, shell_, exitString

    -- * Error handling and control flow
  , (|>), capture, captureStdErr, capture2, capture3, stream, lift
  , try, orElse, exit
  , Guard (..), guard, when, unless

    -- * Environment handling
  , withEnv, withoutEnv, lookupEnv, getEnv, cmdline

    -- * Running external commands
  , MonadIO (..), Env (..)
  , run, sudo
  , unsafeLiftIO
  , absPath, shellEnv, getShellEnv, joinResult, runSh

    -- * Working with directories
  , cpdir, pwd, ls, mkdir, rmdir, inDirectory, isDirectory
  , withHomeDirectory, inHomeDirectory, withAppDirectory, inAppDirectory
  , forEachFile, forEachFile_, forEachDirectory, forEachDirectory_

    -- * Working with files
  , isFile, rm, mv, cp, input, output
  , withFile, withBinaryFile, openFile, openBinaryFile

    -- * Working with temporary files and directories
  , FileMode (..)
  , withTempFile, withCustomTempFile
  , withTempDirectory, withCustomTempDirectory
  , inTempDirectory, inCustomTempDirectory

    -- * Working with handles
  , Handle, IOMode (..), BufferMode (..)
  , hFlush, hClose, hReady
  , hGetBuffering, hSetBuffering
  , getStdIn, getStdOut, getStdErr

    -- * Text I/O
  , hPutStr, hPutStrLn, echo, echo_, ask, stdin
  , hGetLine, hGetContents

    -- * Terminal text formatting
  , module Control.Shell.Color

    -- * ByteString I/O
  , hGetBytes, hPutBytes, hGetByteLine, hGetByteContents

    -- * Convenient re-exports
  , module System.FilePath
  , module Control.Monad
  ) where
import qualified System.Environment as Env
import System.IO.Unsafe
import Control.Shell.Base hiding (getEnv, takeEnvLock, releaseEnvLock, setShellEnv)
import qualified Control.Shell.Base as CSB
import Control.Shell.Handle
import Control.Shell.File
import Control.Shell.Directory
import Control.Shell.Temp
import Control.Shell.Control
import Control.Shell.Color
import Control.Monad hiding (guard, when, unless)
import System.FilePath

-- | Convert an 'ExitReason' into a 'String'. Successful termination yields
--   the empty string, while abnormal termination yields the termination
--   error message. If the program terminaged abnormally but without an error
--   message - i.e. the error message is empty string - the error message will
--   be shown as @"abnormal termination"@.
exitString :: ExitReason -> String
exitString Success      = ""
exitString (Failure "") = "abnormal termination"
exitString (Failure s)  = s

-- | Get the complete environment for the current computation.
getShellEnv :: Shell Env
getShellEnv = CSB.getEnv

insert :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insert k' v' (kv@(k, _) : kvs)
  | k == k'   = (k', v') : kvs
  | otherwise = kv : insert k' v' kvs
insert k v _  = [(k, v)]

delete :: Eq k => k -> [(k, v)] -> [(k, v)]
delete k' (kv@(k, _) : kvs)
  | k == k'   = kvs
  | otherwise = kv : delete k' kvs
delete _ _    = []

-- | The executable's command line arguments.
cmdline :: [String]
cmdline = unsafePerformIO Env.getArgs

-- | Run a computation with the given environment variable set.
withEnv :: String -> String -> Shell a -> Shell a
withEnv k v m = do
  e <- CSB.getEnv
  inEnv (e {envEnvVars = insert k v (envEnvVars e)}) m

-- | Run a computation with the given environment variable unset.
withoutEnv :: String -> Shell a -> Shell a
withoutEnv k m = do
  e <- CSB.getEnv
  inEnv (e {envEnvVars = delete k (envEnvVars e)}) m

-- | Get the value of an environment variable. Returns Nothing if the variable 
--   doesn't exist.
lookupEnv :: String -> Shell (Maybe String)
lookupEnv k = lookup k . envEnvVars <$> CSB.getEnv

-- | Get the value of an environment variable. Returns the empty string if
--   the variable doesn't exist.
getEnv :: String -> Shell String
getEnv key = maybe "" id `fmap` lookupEnv key

-- | Run a command with elevated privileges.
sudo :: FilePath -> [String] -> Shell ()
sudo cmd as = run "sudo" (cmd:"--":as)

-- | Performs a command inside a temporary directory. The directory will be
--   cleaned up after the command finishes.
inTempDirectory :: Shell a -> Shell a
inTempDirectory = withTempDirectory . flip inDirectory

-- | Performs a command inside a temporary directory. The directory will be
--   cleaned up after the command finishes.
inCustomTempDirectory :: FilePath -> Shell a -> Shell a
inCustomTempDirectory dir m = withCustomTempDirectory dir $ flip inDirectory m

-- | Get the standard input, output and error handle respectively.
getStdIn, getStdOut, getStdErr :: Shell Handle
getStdIn  = envStdIn  <$> CSB.getEnv
getStdOut = envStdOut <$> CSB.getEnv
getStdErr = envStdErr <$> CSB.getEnv
