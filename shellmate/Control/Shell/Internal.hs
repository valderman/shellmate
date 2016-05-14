{-# LANGUAGE CPP, GADTs, RecordWildCards, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
-- | Internal, hairy bits of Shellmate.
module Control.Shell.Internal {- (
    MonadIO (..), Shell, ExitReason (..),
    shell, shell_,
    (|>), exit,
    run, run_, genericRun, runInteractive,
    withTempDirectory, withCustomTempDirectory,
    withTempFile, withCustomTempFile,
    try
  ) -} where
import Control.Monad (when, ap, foldM, void, (>=>))
import Control.Monad.IO.Class
import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import Data.List (sort)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import System.IO.Unsafe

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
data CMD a where
  Lift :: !(IO a) -> CMD a
  Pipe :: ![PipeStep] -> CMD ()
  Bind :: CMD a -> (a -> CMD b) -> CMD b
  Done :: CMD a

-- | A step in a pipeline: either a shell computation or an external process.
data PipeStep
  = Proc     !String ![String]
  | Internal !(Shell ())

instance Functor CMD where
  fmap f (Lift m) = Lift (fmap f m)
  fmap f (Pipe p) = Pipe p >> pure (f ())

instance Applicative CMD where
  (<*>) = ap
  pure  = return

instance Monad CMD where
  return = Lift . return
  (>>=) = Bind

instance MonadIO CMD where
  liftIO = Lift

-- | Monad for running shell commands. If a command fails, the entire
--   computation is aborted unless @try@ is used.
newtype Shell a = Shell {unSh :: Env -> CMD a}

instance Functor Shell where
  fmap f (Shell m) = Shell $ fmap f . m

instance Applicative Shell where
  (<*>) = ap
  pure = return

instance Monad Shell where
  return = Shell . const . return
  Shell m >>= f = Shell $ \env -> do
    x <- m env
    unSh (f x) env

instance MonadIO Shell where
  liftIO m = Shell $ const $ liftIO m

data ExitReason = Success | Failure !String
  deriving (Show, Eq)

shell :: Shell a -> IO (Either ExitReason a)
shell = flip runSh (Env IO.stdin IO.stdout IO.stderr)

shell_ :: Shell a -> IO a
shell_ m = do
  mx <- shell m
  case mx of
    Right x          -> return x
    Left (Failure e) -> error e

runSh :: Shell a -> Env -> IO (Either ExitReason a)
runSh (Shell m) env = runCMD env (m env)

runCMD :: Env -> CMD a -> IO (Either ExitReason a)
runCMD env (Lift m) = do
  Ex.catch (Right <$> m)
           (\(Ex.SomeException e) -> pure $ Left (Failure (show e)))
runCMD env (Pipe p) = do
  ((stepenv, step) : steps) <- mkEnvs env p
  ma <- waitPids =<< mapM (uncurry (runStep True)) steps
  mb <- waitPids . (:[]) =<< runStep False stepenv step
  case ma >> mb of
    Just err -> pure $ Left err
    _        -> pure $ Right ()
runCMD _ Done = do
  return $ Left Success
runCMD env (Bind m f) = do
  res <- runCMD env m
  case res of
    Right x -> runCMD env (f x)
    Left e  -> return $ Left e

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
  tid <- Conc.forkFinally (runSh cmd env >>= done) $ \res -> do
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
run p args = Shell $ \env -> Pipe [Proc p args]

-- | Terminate the program successfully.
exit :: Shell a
exit = Shell $ const Done

infixr 5 |>
(|>) :: Shell () -> Shell () -> Shell ()
m |> n = Shell $ \env -> do
  case (unSh m env, unSh n env) of
    (Pipe m', Pipe n') -> Pipe (m' ++ n')
    (Pipe m', _)       -> Pipe (m' ++ [Internal n])
    (_, Pipe n')       -> Pipe (Internal m : n')
    _                  -> Pipe [Internal m, Internal n]

inEnv :: Env -> Shell a -> Shell a
inEnv e m = do
  res <- liftIO $ runSh m e
  case res of
    Right x          -> pure x
    Left Success     -> exit
    Left (Failure e) -> fail e

getEnv :: Shell Env
getEnv = Shell $ \env -> pure env

capture :: Shell () -> Shell String
capture m = do
  env <- getEnv
  (r, w) <- liftIO Proc.createPipe
  inEnv (env {envStdOut = w}) m
  liftIO $ IO.hClose w >> IO.hGetContents r

stream :: (String -> String) -> Shell ()
stream f = withStdOut (f <$> stdin)

lift :: (String -> Shell String) -> Shell ()
lift f = withStdOut (stdin >>= f)

stdin :: Shell String
stdin = Shell $ \env -> Lift $ do
  IO.hGetContents (envStdIn env)

withStdOut :: Shell String -> Shell ()
withStdOut m = do
  s <- m
  Shell $ \env -> Lift $ IO.hPutStr (envStdOut env) s

echo :: String -> Shell ()
echo s = Shell $ \env -> Lift $ IO.hPutStrLn (envStdOut env) s

ask :: Shell String
ask = Shell $ \env -> Lift $ IO.hGetLine (envStdIn env)
