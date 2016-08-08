-- | Daemonize a shellmate computation.
--   Windows compatibility not guaranteed.
module Control.Shell.Daemon
  ( CMode
  , daemonize
  , Control.Shell.Daemon.setFileCreationMask
  ) where
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix
import Control.Shell hiding (stdin)

-- | Daemonize a shellmate computation. This should be the last thing a
--   computation does, as this function will terminate the parent computation.
--   In short, daemonizing a computation involves setting the file creation
--   mask to 0, closing standard input, output and error file descriptors,
--   blocking sighup and changing the working directory to @/@.
daemonize :: Shell () -> Shell ()
daemonize program = do
    env <- getShellEnv
    unsafeLiftIO $ do
      System.Posix.setFileCreationMask 0
      forkProcess (p env)
      exitImmediately ExitSuccess
  where
    p env = do
      createSession
      forkProcess (p' env)
      exitImmediately ExitSuccess
    p' env = do
      changeWorkingDirectory "/"
      null <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
      mapM_ closeFd [stdInput, stdOutput, stdError]
      mapM_ (dupTo null) [stdInput, stdOutput, stdError]
      installHandler sigHUP Ignore Nothing
      dir <- getCurrentDirectory
      res <- flip runSh program $ env
        { envStdIn   = stdin
        , envStdOut  = stdout
        , envStdErr  = stderr
        , envWorkDir = dir
        , envEnvVars = envEnvVars env
        }
      case res of
        Left (Failure _) -> exitFailure
        _                -> exitSuccess

-- | Set the file creation mask of this process.
setFileCreationMask :: CMode -> Shell CMode
setFileCreationMask = unsafeLiftIO . System.Posix.setFileCreationMask
