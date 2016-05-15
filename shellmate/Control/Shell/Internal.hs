{-# LANGUAGE CPP, GADTs, RecordWildCards #-}
-- | Primitives and run function.
module Control.Shell.Internal
  ( Shell
  , ExitReason (..), Env (..)
  , shell
  , exit, run, try, getEnv, inEnv, unsafeLiftIO, (|>)
  ) where
import Control.Monad (when, ap)
import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO as IO

-- | A command name plus a ProcessHandle.
data Pid = PID {pidName :: !String, pidHandle :: !Proc.ProcessHandle}
         | TID !(Conc.MVar (Maybe ExitReason)) !Conc.ThreadId

data Env = Env
  { envStdIn  :: !IO.Handle
  , envStdOut :: !IO.Handle
  , envStdErr :: !IO.Handle
  }

-- | A shell command: either an IO computation or a pipeline of at least one
--   step.
data Shell a where
  Lift   :: !(IO a) -> Shell a
  Pipe   :: ![PipeStep] -> Shell ()
  Bind   :: Shell a -> (a -> Shell b) -> Shell b
  GetEnv :: Shell Env
  InEnv  :: Env -> Shell a -> Shell a
  Try    :: Shell a -> Shell (Either String a)
  Done   :: Shell a

-- | A step in a pipeline: either a shell computation or an external process.
data PipeStep
  = Proc     !String ![String]
  | Internal !(Shell ())

instance Functor Shell where
  fmap f (Lift m) = Lift (fmap f m)
  fmap f (Pipe p) = Pipe p >> pure (f ())

instance Applicative Shell where
  (<*>) = ap
  pure  = return

instance Monad Shell where
  return = Lift . return
  (>>=) = Bind

-- | Lift an IO computation into a shell. The lifted computation is not
--   thread-safe, and should thus absolutely not use environment variables,
--   relative paths or standard input/output.
unsafeLiftIO :: IO a -> Shell a
unsafeLiftIO = Lift

-- | Why did the computation terminate?
data ExitReason = Success | Failure !String
  deriving (Show, Eq)

-- | Run a shell computation. If part of the computation fails, the whole
--   computation fails.
shell :: Shell a -> IO (Either ExitReason a)
shell = runSh (Env IO.stdin IO.stdout IO.stderr)

runSh :: Env -> Shell a -> IO (Either ExitReason a)
runSh env (Lift m) = do
  Ex.catch (Right <$> m)
           (\(Ex.SomeException e) -> pure $ Left (Failure (show e)))
runSh env (Pipe p) = do
  ((stepenv, step) : steps) <- mkEnvs env p
  ma <- waitPids =<< mapM (uncurry (runStep True)) steps
  mb <- waitPids . (:[]) =<< runStep False stepenv step
  case ma >> mb of
    Just err -> pure $ Left err
    _        -> pure $ Right ()
runSh _ Done = do
  return $ Left Success
runSh env (Bind m f) = do
  res <- runSh env m
  case res of
    Right x -> runSh env (f x)
    Left e  -> pure $ Left e
runSh env GetEnv = do
  pure $ Right env
runSh _ (InEnv env m) = do
  runSh env m
runSh env (Try m) = do
  res <- runSh env m
  case res of
    Right x          -> pure $ Right (Right x)
    Left (Failure e) -> pure $ Right (Left e)
    Left Success     -> pure $ Left Success

-- | Start a pipeline step.
runStep :: Bool -> Env -> PipeStep -> IO Pid
runStep closefds Env{..} (Proc cmd args) = do
    (_, _, _, ph) <- Proc.createProcess cproc
    pure $ PID cmd ph
  where
    cproc = Proc.CreateProcess
      { Proc.cmdspec      = Proc.RawCommand cmd args
      , Proc.cwd          = Nothing
      , Proc.env          = Nothing
      , Proc.std_in       = Proc.UseHandle envStdIn
      , Proc.std_out      = Proc.UseHandle envStdOut
      , Proc.std_err      = Proc.UseHandle envStdErr
      , Proc.close_fds    = closefds
#if MIN_VERSION_process(1,2,0)
      , Proc.delegate_ctlc = False
#endif
      , Proc.create_group = False
      }
runStep closefds env (Internal cmd) = do
  v <- Conc.newEmptyMVar
  tid <- Conc.forkFinally (runSh env cmd >>= done) $ \res -> do
    case res of
      Right (Left e) -> Conc.putMVar v (Just e)
      Left e         -> Conc.putMVar v (Just $ Failure $ show e)
      _              -> Conc.putMVar v Nothing
  pure $ TID v tid
  where
    done x = do
      when closefds $ IO.hClose (envStdOut env)
      return x

-- | Pair up pipe steps with corresponding environments, ensuring that each
--   step is connected to the next via a pipe.
mkEnvs :: Env -> [PipeStep] -> IO [(Env, PipeStep)]
mkEnvs env = go [] (envStdIn env)
  where
    go acc stdi (step : steps) = do
      (next, stdo) <- Proc.createPipe
      go ((env {envStdIn = stdi, envStdOut = stdo}, step):acc) next steps
    go ((e, s) : steps) _ _ = do
      pure ((e {envStdOut = envStdOut env}, s) : steps)

-- | Terminate a pid, be it process or thread.
killPid :: Pid -> IO ()
killPid (PID _ p) = Proc.terminateProcess p
killPid (TID _ t) = Conc.killThread t

-- | Wait for all processes in the given list. If a process has failed, its
--   error message is returned and the rest are killed.
waitPids :: [Pid] -> IO (Maybe ExitReason)
waitPids (PID cmd p : ps) = do
  exCode <- Proc.waitForProcess p
  case exCode of
    Exit.ExitFailure ec -> do
      mapM_ killPid ps
      return . Just $ Failure $ "Command '" ++ cmd ++ "' failed with error "
                              ++" code " ++ show ec
    _ -> do
      waitPids ps
waitPids (TID v t : ps) = do
  merr <- Conc.takeMVar v
  case merr of
    Just e -> mapM_ killPid ps >> return (Just e)
    _      -> waitPids ps
waitPids _ = do
  return Nothing

-- | Execute an external command. No globbing, escaping or other external shell
--   magic is performed on either the command or arguments. The program's
--   stdout will be written to stdout.
run :: FilePath -> [String] -> Shell ()
run p args = Pipe [Proc p args]

-- | Terminate the program successfully.
exit :: Shell a
exit = Done

-- | Connect the standard output of the first argument to the standard input
--   of the second argument, and run the two computations in parallel.
(|>) :: Shell () -> Shell () -> Shell ()
Pipe m |> Pipe n = Pipe (m ++ n)
Pipe m |> n      = Pipe (m ++ [Internal n])
m      |> Pipe n = Pipe (Internal m : n)
m      |> n      = Pipe [Internal m, Internal n]
infixl 5 |>

-- | Run a computation in the given environment.
inEnv :: Env -> Shell a -> Shell a
inEnv = InEnv

-- | Get the current environment.
getEnv :: Shell Env
getEnv = GetEnv

-- | Attempt to run a computation. If the inner computation fails, the outer
--   computations returns its error message, otherwise its result is returned.
try :: Shell a -> Shell (Either String a)
try = Try