-- | Basic operations such as reading input/output, etc.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Shell.Base
  ( module Control.Shell.Internal
  , IO.Handle, FileMode (..)
  , MonadIO (..), shellEnv
  , shell_
  , stdin, echo, echo_, ask
  , capture, stream, lift
  , takeEnvLock, releaseEnvLock, setShellEnv, joinResult
  ) where
import qualified System.Process as Proc
import qualified System.IO as IO
import qualified Control.Concurrent as Conc
import qualified System.Directory as Dir
import qualified System.Environment as Env
import Data.IORef
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Shell.Internal

-- | Perform a file operation in binary or text mode?
data FileMode = BinaryMode | TextMode
  deriving (Show, Eq)

{-# NOINLINE globalEnvLock #-}
globalEnvLock :: Conc.MVar ()
globalEnvLock = unsafePerformIO $ Conc.newMVar ()

{-# NOINLINE globalEnv #-}
globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

-- | Take the global environment lock.
takeEnvLock :: Shell ()
takeEnvLock = unsafeLiftIO $ Conc.takeMVar globalEnvLock

-- | Release the global environment lock.
releaseEnvLock :: Shell ()
releaseEnvLock = unsafeLiftIO $ Conc.putMVar globalEnvLock ()

-- | Set the global shell environment to the one of the current computation.
--   Returns the current global environment.
--   Should never, ever, be called without holding the global environment lock.
setShellEnv :: Env -> Shell Env
setShellEnv env = unsafeLiftIO $ do
  evs <- Env.getEnvironment
  wd <- Dir.getCurrentDirectory
  writeIORef globalEnv env
  Dir.setCurrentDirectory (envWorkDir env)
  mapM_ Env.unsetEnv (map fst evs)
  mapM_ (uncurry Env.setEnv) (envEnvVars env)
  return $ Env IO.stdin IO.stdout IO.stderr wd evs

-- | Get the current global shell environment, including standard input,
--   output and error handles. Only safe to call within a computation lifted
--   into 'Shell' by 'liftIO'.
shellEnv :: IO Env
shellEnv = readIORef globalEnv

instance MonadIO Shell where
  liftIO m = do
    env <- getEnv
    unsafeLiftIO $ Conc.withMVar globalEnvLock $ \_ -> do
      writeIORef globalEnv env
      m

-- | Run a shell computation and return its result. If the computation calls
--   'exit', the return value will be undefined. If the computation fails,
--   an error will be thrown.
shell_ :: Shell a -> IO a
shell_ m = do
  res <- shell m
  case res of
    Right x          -> pure x
    Left Success     -> pure $ error "Shell computation terminated successfully"
    Left (Failure e) -> error e

-- | Perform the given computation and return its standard output.
capture :: Shell () -> Shell String
capture m = do
  env <- getEnv
  (r, w) <- unsafeLiftIO Proc.createPipe
  inEnv (env {envStdOut = w}) m
  unsafeLiftIO $ IO.hClose w >> IO.hGetContents r

-- | Lift a pure function to a computation over standard input/output.
--   Similar to 'interact'.
stream :: (String -> String) -> Shell ()
stream f = stdin >>= echo_ . f

-- | Lift a shell computation to a function over stdin and stdout.
--   Similar to 'interact'.
lift :: (String -> Shell String) -> Shell ()
lift f = stdin >>= f >>= echo_

-- | Get the contents of the computation's standard input.
stdin :: Shell String
stdin = getEnv >>= unsafeLiftIO . IO.hGetContents . envStdIn

-- | Write a string to standard output, followed by a newline.
echo :: String -> Shell ()
echo s = getEnv >>= unsafeLiftIO . flip IO.hPutStrLn s . envStdOut

-- | Write a string to standard output, without appending a newline.
echo_ :: String -> Shell ()
echo_ s = getEnv >>= unsafeLiftIO . flip IO.hPutStr s . envStdOut

-- | Read a line of text from standard input.
ask :: Shell String
ask = getEnv >>= unsafeLiftIO . IO.hGetLine . envStdIn

-- | Propagate an explicit 'ExitResult' through the computation.
joinResult :: Shell (Either ExitReason a) -> Shell a
joinResult m = do
  res <- m
  case res of
    Right x          -> pure x
    Left Success     -> exit
    Left (Failure e) -> fail e
