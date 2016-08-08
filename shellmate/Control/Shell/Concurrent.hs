{-# LANGUAGE CPP #-}
-- | Concurrency for Shellmate programs.
module Control.Shell.Concurrent (
    Future, ThreadId,
    Control.Shell.Concurrent.forkIO, Control.Shell.Concurrent.killThread,
    fork2, fork3,
    future, await, check,
    parallel, parallel_,
    chunks
  ) where
import Control.Concurrent as CC
import Control.Monad
import Control.Shell
import Control.Shell.Internal (inEnv)
import Data.IORef
import System.Process

-- | Only used to have something reliable to attach the futures' weakrefs to.
type FinalizerHandle = IORef ThreadId

-- | A future is a computation which is run in parallel with a program's main
--   thread and which may at some later point finish and return a value.
--
--   Note that future computations are killed when their corresponding @Future@
--   is garbage collected. This means that a future should *always* be
--   'await'ed at some point or otherwise kept alive, to ensure that the
--   computation finishes.
--
--   Note that all any code called in a future using 'unsafeLiftIO' must
--   refrain from using environment variables, standard input/output, relative
--   paths and the current working directory, in order to avoid race
--   conditions. Code within the 'Shell' monad, code imported using 'liftIO'
--   and external processes spawned from within 'Shell' is perfectly safe.
data Future a = Future !FinalizerHandle !(MVar (Either ExitReason a))

-- | Create a future value.
future :: Shell a -> Shell (Future a)
future m = do
  env <- getShellEnv
  unsafeLiftIO $ do
    v <- newEmptyMVar
    tid <- CC.forkIO $ runSh env m >>= putMVar v
    -- We need a WeakRef to something that's not referenced by the computation
    -- to be able to kill it when the result is not reachable. IORef to TID is
    -- as good as anything.
    r <- newIORef tid
    _ <- mkWeakIORef r (CC.killThread tid)
    return $ Future r v

-- | Wait for a future value.
await :: Future a -> Shell a
await (Future h v) = joinResult $ unsafeLiftIO (readMVar v <* readIORef h)

-- | Check whether a future value has arrived or not.
check :: Future a -> Shell (Maybe a)
check (Future h v) = do
  mx <- unsafeLiftIO $ tryReadMVar v <* readIORef h
  case h `seq` mx of
    Just x -> Just <$> joinResult (pure x)
    _      -> pure Nothing

-- | Perform the given computations in parallel.
--   The race condition warning for 'Future' when modifying environment
--   variables or using relative paths still applies.
parallel :: [Shell a] -> Shell [a]
parallel = mapM future >=> mapM await

-- | Like 'parallel', but discards any return values.
parallel_ :: [Shell a] -> Shell ()
parallel_ = mapM future >=> mapM_ await

-- | Break a list into chunks. This is quite useful for when performing *every*
--   computation in parallel is too much. For instance, to download a list of
--   files three at a time, one would do
--   @mapM_ (parallel_ downloadFile) (chunks 3 files)@.
chunks :: Int -> [a] -> [[a]]
chunks _ []                 = []
chunks n xs | length xs > n = take n xs : chunks n (drop n xs)
            | otherwise     = [xs]

-- | Run a computation in a separate thread. If the thread throws an error,
--   it will simply die; the error will not propagate to its parent thread.
forkIO :: Shell () -> Shell ThreadId
forkIO m = do
  env <- getShellEnv
  unsafeLiftIO $ do
    CC.forkIO $ void $ runSh env m

-- | Run a computation in a separate thread, with its standard input and output
--   provided by the first and second handles returned respectively.
--   The handles are line buffered by default.
fork2 :: Shell () -> Shell (Handle, Handle, ThreadId)
fork2 m = do
  env <- getShellEnv
  (ri, wi) <- unsafeLiftIO createPipe
  (ro, wo) <- unsafeLiftIO createPipe
  mapM_ (flip hSetBuffering LineBuffering) [wi,wo]
  tid <- Control.Shell.Concurrent.forkIO $ do
    inEnv (env {envStdOut = wo, envStdIn = ri}) m
  return (wi, ro, tid)

-- | Like 'fork2', but adds a third handle for standard error.
fork3 :: Shell () -> Shell (Handle, Handle, Handle, ThreadId)
fork3 m = do
  env <- getShellEnv
  (ri, wi) <- unsafeLiftIO createPipe
  (ro, wo) <- unsafeLiftIO createPipe
  (re, we) <- unsafeLiftIO createPipe
  mapM_ (flip hSetBuffering LineBuffering) [wi,wo,we]
  tid <- inEnv (env {envStdOut = wo, envStdErr = we, envStdIn = ri}) $ do
    Control.Shell.Concurrent.forkIO m
  return (wi, ro, re, tid)

-- | Terminate a thread spawned by 'Control.Shell.Concurrent.forkIO'.
killThread :: ThreadId -> Shell ()
killThread = liftIO . CC.killThread
