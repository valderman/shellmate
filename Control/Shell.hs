{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable,
             MultiParamTypeClasses, FunctionalDependencies,
             CPP,
             UndecidableInstances #-}
-- | Simple interface for shell scripting-like tasks.
module Control.Shell (
    -- * Basic types and classes
    Shell,

    -- * Running Shell programs
    shell, (|>),

    -- * Error handling
    Guard (..),
    try, orElse,

    -- * Environment handling
    setEnv, getEnv, withEnv, lookupEnv,

    -- * Running commands
    MonadIO (..),
    run, run_, runInteractive, sudo,

    -- * Working with directories
    cd, cpDir, pwd, ls, mkdir, rmdir, inDirectory, isDirectory,
    withHomeDirectory, inHomeDirectory, withAppDirectory, inAppDirectory,
    forEachFile, forEachFile_,

    -- * Working with files
    isFile, rm, mv, cp, readf, writef,

    -- * Working with temporary files and directories
    withTempFile, withCustomTempFile,
    withTempDirectory, withCustomTempDirectory, inTempDirectory,

    -- * Text I/O
    IO.Handle,
    IO.stdin, IO.stdout, IO.stderr,
    hPutStr, hPutStrLn, hClose, echo,

    -- * @FilePath@s
    module System.FilePath
  ) where
import Control.Applicative
import Control.Monad (forM, filterM, forM_)
import System.FilePath
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.IO as IO
import Control.Shell.Internal

-- | Lazily read a file.
readf :: MonadIO m => FilePath -> m String
readf = liftIO . readFile

-- | Lazily write a file.
writef :: MonadIO m => FilePath -> String -> m ()
writef f = liftIO . writeFile f

-- | Set an environment variable.
setEnv :: MonadIO m => String -> String -> m ()
setEnv k v = liftIO $ Env.setEnv k v

-- | Get the value of an environment variable. Returns Nothing if the variable 
--   doesn't exist.
lookupEnv :: MonadIO m => String -> m (Maybe String)
lookupEnv = liftIO . Env.lookupEnv

-- | Run a computation with a new value for an environment variable.
--   Note that this will *not* affect external commands spawned using @liftIO@
--   or which directory is considered the system temp directory.
withEnv :: MonadIO m => String -> (String -> String) -> m a -> m a
withEnv key f act = do
  v <- lookupEnv key
  setEnv key $ f (maybe "" id v)
  x <- act
  maybe (return ()) (setEnv key) v
  return x

-- | Get the value of an environment variable. Returns the empty string if
--   the variable doesn't exist.
getEnv :: MonadIO m => String -> m String
getEnv key = maybe "" id `fmap` lookupEnv key

-- | Run a command with elevated privileges.
sudo :: FilePath -> [String] -> String -> Shell String
sudo cmd args stdin = run "sudo" (cmd:args) stdin

-- | Change working directory.
cd :: MonadIO m => FilePath -> m ()
cd = liftIO . Dir.setCurrentDirectory

-- | Get the current working directory.
pwd :: MonadIO m => m FilePath
pwd = liftIO $ Dir.getCurrentDirectory

-- | Remove a file.
rm :: MonadIO m => FilePath -> m ()
rm = liftIO . Dir.removeFile

-- | Rename a file.
mv :: MonadIO m => FilePath -> FilePath -> m ()
mv from to = liftIO $ Dir.renameFile from to

-- | Recursively copy a directory. If the target is a directory that already
--   exists, the source directory is copied into that directory using its
--   current name.
cpDir :: MonadIO m => FilePath -> FilePath -> m ()
cpDir from to = do
  todir <- isDirectory to
  if todir
    then do
      cpDir from (to </> takeBaseName from)
    else do
      cpfile <- isFile from
      if cpfile
        then do
          cp from to
        else do
          liftIO $ Dir.createDirectoryIfMissing False to
          ls from >>= mapM_ (\f -> cpDir (from </> f) (to </> f))

-- | Perform an action on each file in the given directory.
--   This function will traverse any subdirectories of the given as well.
--   File paths are given relative to the given directory; the current working
--   directory is not affected.
forEachFile :: MonadIO m => FilePath -> (FilePath -> m a) -> m [a]
forEachFile dir f = do
  files <- map (dir </>) <$> ls dir
  xs <- filterM isFile files >>= mapM f
  fromdirs <- filterM isDirectory files
  xss <- forM fromdirs $ \d -> do
    forEachFile d f
  return $ concat (xs:xss)

-- | Like @forEachFile@ but only performs a side effect.
forEachFile_ :: MonadIO m => FilePath -> (FilePath -> m ()) -> m ()
forEachFile_ dir f = do
  files <- map (dir </>) <$> ls dir
  filterM isFile files >>= mapM_ f
  fromdirs <- filterM isDirectory files
  forM_ fromdirs $ \d -> do
    forEachFile d f

-- | Copy a file. Fails if the source is a directory. If the target is a
--   directory, the source file is copied into that directory using its current
--   name.
cp :: MonadIO m => FilePath -> FilePath -> m ()
cp from to = do
  todir <- isDirectory to
  if todir
    then cp from (to </> takeFileName from)
    else liftIO $ Dir.copyFile from to

-- | List the contents of a directory, sans '.' and '..'.
ls :: MonadIO m => FilePath -> m [FilePath]
ls dir = do
  contents <- liftIO $ Dir.getDirectoryContents dir
  return [f | f <- contents, f /= ".", f /= ".."]

-- | Create a directory. Optionally create any required missing directories as
--   well.
mkdir :: MonadIO m => Bool -> FilePath -> m ()
mkdir True = liftIO . Dir.createDirectoryIfMissing True
mkdir _    = liftIO . Dir.createDirectory

-- | Recursively remove a directory. Follows symlinks, so be careful.
rmdir :: MonadIO m => FilePath -> m ()
rmdir = liftIO . Dir.removeDirectoryRecursive

-- | Do something with the user's home directory.
withHomeDirectory :: MonadIO m => (FilePath -> m a) -> m a
withHomeDirectory act = liftIO Dir.getHomeDirectory >>= act

-- | Perform an action with the user's home directory as the working directory.
inHomeDirectory :: MonadIO m => m a -> m a
inHomeDirectory act = withHomeDirectory $ flip inDirectory act

-- | Do something with the given application's data directory.
withAppDirectory :: MonadIO m => String -> (FilePath -> m a) -> m a
withAppDirectory app act = liftIO (Dir.getAppUserDataDirectory app) >>= act

-- | Do something with the given application's data directory as the working
--   directory.
inAppDirectory :: MonadIO m => FilePath -> m a -> m a
inAppDirectory app act = withAppDirectory app $ flip inDirectory act

-- | Execute a command in the given working directory, then restore the
--   previous working directory.
inDirectory :: MonadIO m => FilePath -> m a -> m a
inDirectory dir act = do
  curDir <- pwd
  cd dir
  x <- act
  cd curDir
  return x

-- | Does the given path lead to a directory?
isDirectory :: MonadIO m => FilePath -> m Bool
isDirectory = liftIO . Dir.doesDirectoryExist

-- | Does the given path lead to a file?
isFile :: MonadIO m => FilePath -> m Bool
isFile = liftIO . Dir.doesFileExist

-- | Performs a command inside a temporary directory. The directory will be
--   cleaned up after the command finishes.
inTempDirectory :: Shell a -> Shell a
inTempDirectory = withTempDirectory "shellmate" . flip inDirectory

-- | Attempt to run the first command. If the first command fails, run the
--   second. Forces serialization of the first command.
orElse :: Shell a -> Shell a -> Shell a
orElse a b = do
  ex <- try a
  case ex of
    Right x -> return x
    _       -> b

-- | Write a string to a handle.
hPutStr :: MonadIO m => IO.Handle -> String -> m ()
hPutStr h s = liftIO $ IO.hPutStr h s

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: MonadIO m => IO.Handle -> String -> m ()
hPutStrLn h s = liftIO $ IO.hPutStrLn h s

-- | Close a handle.
hClose :: MonadIO m => IO.Handle -> m ()
hClose = liftIO . IO.hClose

-- | Write a string to @stdout@ followed by a newline.
echo :: MonadIO m => String -> m ()
echo = liftIO . putStrLn

class Guard guard a | guard -> a where
  -- | Perform a Shell computation; if the computation succeeds but returns
  --   a false-ish value, the outer Shell computation fails with the given
  --   error message.
  guard :: String -> guard -> Shell a

instance Guard (Maybe a) a where
  guard _ (Just x) = return x
  guard desc _     = fail $ "Guard failed: " ++ desc

instance Guard Bool () where
  guard _ True = return ()
  guard desc _ = fail $ "Guard failed: " ++ desc

instance Guard a b => Guard (Shell a) b where
  guard desc m = m >>= \x -> guard desc x
