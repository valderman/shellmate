-- | Concurrency for Shellmate programs.
module Control.Shell.Concurrent (
    Future,
    future, await, check, parallel
  ) where
import Control.Concurrent
import Control.Monad
import Control.Shell
import Data.IORef

type FinalizerHandle = IORef Int

-- | A future is a computation which is run in parallel with a program's main
--   thread and which may at some later point finish and return a value.
--
--   Note that future computations are killed when their corresponding @Future@
--   is garbage collected. This means that a future should *always* be
--   'await'ed at some point or otherwise kept alive, to ensure that the
--   computation finishes.
data Future a = Future !FinalizerHandle !(MVar (Either ExitReason a))

-- | Create a future value.
future :: Shell a -> Shell (Future a)
future m = liftIO $ do
  v <- newEmptyMVar
  tid <- forkIO $ shell m >>= putMVar v
  r <- newIORef 0
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
await (Future h v) = liftIO (readMVar v) >>= (h `seq` fromResult)

-- | Check whether a future value has arrived or not.
check :: Future a -> Shell (Maybe a)
check (Future h v) = do
  mx <- liftIO $ tryReadMVar v
  maybe (pure Nothing) (fmap Just . fromResult) (h `seq` mx)

-- | Perform the given computations in parallel.
parallel :: [Shell a] -> Shell [a]
parallel ms = mapM (future >=> await) ms
