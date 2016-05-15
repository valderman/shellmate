{-# LANGUAGE CPP #-}
-- | Concurrency for Shellmate programs.
module Control.Shell.Concurrent (
    Future,
    future, await, check,
    parallel, parallel_,
    chunks
  ) where
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
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
--   Note that all threads running in the same process share the same working
--   directory and environment. It is thus highly inadvisable to change
--   environment variables or the current working directory, or use relative
--   paths from futures, as this will almost certainly lead to race conditions.
data Future a = Future !FinalizerHandle !(MVar (Either ExitReason a))

-- | Create a future value.
future :: Shell a -> Shell (Future a)
future m = liftIO $ do
  v <- newEmptyMVar
  tid <- forkIO $ shell m >>= putMVar v
  -- We need a WeakRef to something that's not referenced by the computation
  -- to be able to kill it when the result is not reachable. IORef to TID is
  -- as good as anything.
  r <- newIORef tid
  _ <- mkWeakIORef r (killThread tid)
  return $ Future r v

-- | Inspect a result from a previous shell computation and make it the result
--   of this computation as well.
fromResult :: Either ExitReason a -> Shell a
fromResult x =
  case x of
    Left Success       -> exit
    Left (Failure err) -> fail err
    Right x'           -> return x'

-- | Wait for a future value.
await :: Future a -> Shell a
await (Future h v) = liftIO (readMVar v <* readIORef h) >>= fromResult

-- | Check whether a future value has arrived or not.
check :: Future a -> Shell (Maybe a)
check (Future h v) = do
  mx <- liftIO $ tryReadMVar v <* readIORef h
  maybe (pure Nothing) (fmap Just . fromResult) (h `seq` mx)

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