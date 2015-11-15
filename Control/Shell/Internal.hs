{-# LANGUAGE CPP #-}
module Control.Shell.Internal (
    MonadIO (..), Shell,
    shell, shell_,
    (|>),
    run, run_, runInteractive,
    withTempDirectory, withCustomTempDirectory,
    withTempFile, withCustomTempFile,
    try
  ) where
import Control.Monad (ap, forM, filterM, forM_, when)
import Control.Monad.IO.Class
import Data.Typeable
import qualified Control.Concurrent as Conc
import qualified Control.Exception as Ex
import qualified Data.Map as M
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc
import qualified System.IO as IO
import qualified System.IO.Temp as Temp

newtype ShellException = ShellException String deriving (Typeable, Show)
instance Ex.Exception ShellException

-- | A command name plus a ProcessHandle.
data Pid = Pid {pidName :: String, pidHandle :: Proc.ProcessHandle}

-- | Monad for running shell commands. If a command fails, the entire
--   computation is aborted unless @mayFail@ is used.
newtype Shell a = Shell {
    unSh :: IO ([Pid], Either String a)
  }

instance Monad Shell where
  fail err = Shell $ return ([], Left err)
  return x = Shell $ return ([], Right x)
  -- | The bind operation of the Shell monad is effectively a barrier; all
  --   commands on the left hand side of a bind will complete before any
  --   command on the right hand side is attempted.
  --   To lazily stream data between two commands, use the @|>@ combinator.
  (Shell m) >>= f = Shell $ do
    (pids, x) <- m
    merr <- waitPids pids
    case (x, merr) of
      (Left err, _) -> return ([], Left err)
      (_, Just err) -> return ([], Left err)
      (Right x', _) -> unSh (f x')

instance MonadIO Shell where
  liftIO act = Shell $ flip Ex.catch exHandler $ do
    x <- act
    return ([], Right x)

instance Applicative Shell where
  pure  = return
  (<*>) = ap

instance Functor Shell where
  fmap f (Shell x) = Shell (fmap (fmap (fmap f)) x)

-- | Run a Shell computation. The program's working directory and environment
--   will be restored after after the computation finishes.
shell :: Shell a -> IO (Either String a)
shell act = do
    dir <- Dir.getCurrentDirectory
    env <- M.fromList <$> Env.getEnvironment
    (pids, res) <- unSh act
    merr <- waitPids pids
    Dir.setCurrentDirectory dir
    resetEnv env
    case merr of
      Just err -> return $ Left err
      _        -> return res
  where
    resetEnv old = do
      new <- M.fromList <$> Env.getEnvironment
      mapM_ (Env.unsetEnv . fst) (M.toList (new M.\\ old))
      mapM_ (uncurry Env.setEnv) (M.toList (M.filterWithKey (changed old) new))
    changed old k v = maybe True (/= v) (M.lookup k old)

-- | Run a shell computation and discard its return value. If the computation
--   fails, print its error message to @stderr@ and exit.
shell' :: Shell a -> IO ()
shell' act = do
  res <- shell act
  case res of
    Left err -> IO.hPutStrLn stderr err >> Exit.exitFailure
    _        -> return ()

-- | Lazy counterpart to monadic bind. To stream data from a command 'a' to a
--   command 'b', do 'a |> b'.
(|>) :: Shell String -> (String -> Shell a) -> Shell a
(Shell m) |> f = Shell $ do
  (pids, x) <- m
  (pids', x') <- case x of
    Left err -> return ([], Left err)
    Right x' -> unSh (f x')
  return (pids ++ pids', x')
infixl 1 |>

-- | Create a temp directory in the standard system temp directory, do
--   something with it, then remove it.
withTempDirectory :: String -> (FilePath -> Shell a) -> Shell a
withTempDirectory template act = Shell $ do
    Temp.withSystemTempDirectory template act'
  where
    act' fp = Ex.catch (unSh (act fp)) exHandler

-- | Create a temp directory in given directory, do something with it, then
--   remove it.
withCustomTempDirectory :: FilePath -> (FilePath -> Shell a) -> Shell a
withCustomTempDirectory dir act = Shell $ do
    Temp.withTempDirectory dir "shellmate" act'
  where
    act' fp = Ex.catch (unSh (act fp)) exHandler

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withTempFile :: String -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withTempFile template act = Shell $ do
    Temp.withSystemTempFile template act'
  where
    act' fp h = Ex.catch (unSh (act fp h)) exHandler

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withCustomTempFile :: FilePath -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withCustomTempFile dir act = Shell $ do
    Temp.withTempFile dir "shellmate" act'
  where
    act' fp h = Ex.catch (unSh (act fp h)) exHandler

-- | Perform an action that may fail without aborting the entire computation.
--   Forces serialization.
try :: Shell a -> Shell (Either String a)
try (Shell act) = Shell $ do
  (pids, x) <- Ex.catch act exHandler
  merr <- waitPids pids
  case merr of
    Just err -> return ([], Right (Left err))
    _        -> return ([], Right x)

-- | Wait for all processes in the given list. If a process has failed, its
--   error message is returned and the rest are killed.
waitPids :: [Pid] -> IO (Maybe String)
waitPids (p:ps) = do
  exCode <- Proc.waitForProcess (pidHandle p)
  case exCode of
    Exit.ExitFailure ec -> do
      killPids ps
      return . Just $ "Command '" ++ (pidName p) ++ "' failed with error "
                    ++" code " ++ show ec
    _ -> do
      waitPids ps
waitPids _ = do
  return Nothing

-- | Kill all processes in the list.
killPids :: [Pid] -> IO ()
killPids = mapM_ (Proc.terminateProcess . pidHandle)

-- | General exception handler; any exception causes failure.
exHandler :: Ex.SomeException -> IO ([Pid], Either String a)
exHandler x = return ([], Left $ show x)

-- | Like 'run', but echoes the command's text output to the screen instead of
--   returning it.
run_ :: FilePath -> [String] -> String -> Shell ()
run_ p args stdin = do
  exCode <- liftIO $ do
    (Just inp, _, pid) <- runP p args Proc.CreatePipe Proc.Inherit
    IO.hPutStr inp stdin
    IO.hClose inp
    Proc.waitForProcess pid
  case exCode of
    Exit.ExitFailure ec -> fail $ "Command '" ++ p ++ "' failed with error " 
                                ++" code " ++ show ec
    _                   -> return ()

-- | Run an interactive process.
runInteractive :: FilePath -> [String] -> Shell ()
runInteractive p args = do
  exCode <- liftIO $ do
    (_, _, pid) <- runP p args Proc.Inherit Proc.Inherit
    Proc.waitForProcess pid
  case exCode of
    Exit.ExitFailure ec -> fail (show ec)
    _                   -> return ()

-- | Execute an external command. No globbing, escaping or other external shell
--   magic is performed on either the command or arguments. The program's text
--   output will be returned, and not echoed to the screen.
run :: FilePath -> [String] -> String -> Shell String
run p args stdin = do
  (output, pid) <- liftIO $ do
    (Just inp, Just out, pid) <- runP p args Proc.CreatePipe Proc.CreatePipe
    let feed str = do
          case splitAt 4096 str of
            ([], [])      -> IO.hClose inp
            (first, str') -> IO.hPutStr inp first >> feed str'
    Conc.forkIO $ feed stdin
    output <- IO.hGetContents out
    return (output, pid)
  output `seq` Shell $ return ([Pid p pid], Right output)

-- | Create a process. Helper for 'run' and friends.
runP :: String
     -> [String]
     -> Proc.StdStream
     -> Proc.StdStream
     -> IO (Maybe IO.Handle, Maybe IO.Handle, Proc.ProcessHandle)
runP p args stdin stdout = do
    (inp, out, _, pid) <- Proc.createProcess cproc
    return (inp, out, pid)
  where
    cproc = Proc.CreateProcess {
        Proc.cmdspec      = Proc.RawCommand p args,
        Proc.cwd          = Nothing,
        Proc.env          = Nothing,
        Proc.std_in       = stdin,
        Proc.std_out      = stdout,
        Proc.std_err      = Proc.Inherit,
        Proc.close_fds    = False,
#if MIN_VERSION_process(1,2,0)
        Proc.delegate_ctlc = False,
#endif
        Proc.create_group = False
      }
