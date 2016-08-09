{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- | Daemonize a shellmate computation.
--   Windows compatibility only with Cygwin.
module Control.Shell.Daemon (daemonize) where
import System.Directory
import System.Exit
import System.IO
import Control.Shell hiding (stdin)

#ifdef WITH_POSIX
import System.Posix
#else
#include "windows_cconv.h"
foreign import WINDOWS_CCONV unsafe "windows.h FreeConsole"
        freeConsole :: IO ()
#endif

-- | Daemonize a shellmate computation. This should be the last thing a
--   computation does, as this function will terminate the parent computation.
--   In short, daemonizing a computation involves setting the file creation
--   mask to 0, closing standard input, output and error file descriptors,
--   blocking sighup and changing the working directory to @/@.
--
--   On Windows without Cygwin, @daemonize@ only detaches the process from the
--   console in which it is running. On Windows, consider running any program
--   intended to be deamonized using @START /B your_app@, or better yet,
--   rewrite it as a Windows service.
--   Note that any non-service process will still be terminated if the starting
--   user logs out.
--
--   With Cygwin, this function behaves as on non-Windows platforms.
daemonize :: Shell () -> Shell ()
#ifdef WITH_POSIX
daemonize m = do
    env <- getShellEnv
    unsafeLiftIO $ do
      void $ setFileCreationMask 0
      void $ forkProcess (p env)
      exitImmediately ExitSuccess
  where
    p env = do
      void $ createSession
      void $ forkProcess (p' env)
      exitImmediately ExitSuccess
    p' env = do
      changeWorkingDirectory "/"
      devnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
      mapM_ closeFd [stdInput, stdOutput, stdError]
      mapM_ (dupTo devnull) [stdInput, stdOutput, stdError]
      void $ installHandler sigHUP Ignore Nothing
      dir <- getCurrentDirectory
      res <- flip runSh m $ env
        { envStdIn   = stdin
        , envStdOut  = stdout
        , envStdErr  = stderr
        , envWorkDir = dir
        , envEnvVars = envEnvVars env
        }
      case res of
        Left (Failure _) -> exitFailure
        _                -> exitSuccess
#else
daemonize m = freeConsole >> m
#endif
