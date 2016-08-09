{-# LANGUAGE CPP #-}
-- | Daemonize a shellmate computation.
--   Windows compatibility only with Cygwin.
module Control.Shell.Daemon (daemonize) where
import Control.Shell hiding (stdin)

#ifdef WITH_POSIX
import System.Directory
import System.Exit
import System.IO
import System.Posix
#endif

-- | Daemonize a shellmate computation. This should be the last thing a
--   computation does, as this function will terminate the parent computation.
--   In short, daemonizing a computation involves setting the file creation
--   mask to 0, closing standard input, output and error file descriptors,
--   blocking sighup and changing the working directory to @/@.
--
--   On Windows without Cygwin, @daemonize@ is a no-op. Consider running any
--   program intended to be deamonized using @START /B your_app@, or better yet,
--   rewriting it as a Windows service.
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
daemonize m = m
#endif
