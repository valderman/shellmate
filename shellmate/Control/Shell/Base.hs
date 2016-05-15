-- | Basic operations such as reading input/output, etc.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Shell.Base
  ( MonadIO (..), shellEnv
  , shell_
  , stdin, echo, echo_, ask
  , capture, stream, lift  
  ) where
import qualified System.Process as Proc
import qualified System.IO as IO
import qualified Control.Concurrent as Conc
import Data.IORef
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Shell.Internal

{-# NOINLINE globalEnvLock #-}
globalEnvLock :: Conc.MVar ()
globalEnvLock = unsafePerformIO $ Conc.newMVar ()

{-# NOINLINE globalEnv #-}
globalEnv :: IORef Env
globalEnv = unsafePerformIO $ newIORef undefined

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
