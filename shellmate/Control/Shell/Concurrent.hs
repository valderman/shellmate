{-# LANGUAGE CPP #-}
-- | Concurrency for Shellmate programs.
module Control.Shell.Concurrent (
    Future,
    future, await, check,
    parallel, parallel_,
    chunks
  ) where
import Control.Concurrent
import Control.Monad
import Control.Shell
import Data.IORef

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
    tid <- forkIO $ runSh env m >>= putMVar v
    -- We need a WeakRef to something that's not referenced by the computation
    -- to be able to kill it when the result is not reachable. IORef to TID is
    -- as good as anything.
    r <- newIORef tid
    _ <- mkWeakIORef r (killThread tid)
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
