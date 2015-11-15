{-# LANGUAGE TypeFamilies #-}
-- | Simple interface for shell scripting-like tasks.
module Control.Shell (
    -- * Running Shell programs
    Shell, ExitReason (..),
    shell, shell_,

    -- * Error handling and control flow
    (|>),
    try, orElse, exit,
    Guard (..), guard, when, unless,

    -- * Environment handling
    setEnv, getEnv, withEnv, lookupEnv, cmdline,

    -- * Running commands
    MonadIO (..),
    run, run_, runInteractive, sudo,

    -- * Working with directories
    cd, cpdir, pwd, ls, mkdir, rmdir, inDirectory, isDirectory,
    withHomeDirectory, inHomeDirectory, withAppDirectory, inAppDirectory,
    forEachFile, forEachFile_, forEachDirectory, forEachDirectory_,

    -- * Working with files
    isFile, rm, mv, cp, input, output,

    -- * Working with temporary files and directories
    withTempFile, withCustomTempFile,
    withTempDirectory, withCustomTempDirectory, inTempDirectory,

    -- * Text I/O
    IO.Handle,
    IO.stdin, IO.stdout, IO.stderr,
    hPutStr, hPutStrLn, hClose, echo, ask,

    -- * Convenient re-exports
    module System.FilePath,
    module Control.Monad
  ) where
import Control.Applicative
import Control.Monad hiding (guard, when, unless)
import System.FilePath
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.IO as IO
import System.IO.Unsafe
import Control.Shell.Internal

-- | Lazily read a file.
input :: FilePath -> Shell String
input = liftIO . readFile

-- | Lazily write a file.
output :: MonadIO m => FilePath -> String -> m ()
output f = liftIO . writeFile f

-- | The executable's command line arguments.
cmdline :: [String]
cmdline = unsafePerformIO Env.getArgs

-- | Set an environment variable.
setEnv :: MonadIO m => String -> String -> m ()
setEnv k v = liftIO $ Env.setEnv k v

-- | Get the value of an environment variable. Returns Nothing if the variable 
--   doesn't exist.
lookupEnv :: String -> Shell (Maybe String)
lookupEnv = liftIO . Env.lookupEnv

-- | Run a computation with a new value for an environment variable.
--   Note that this will *not* affect external commands spawned using @liftIO@
--   or which directory is considered the system temp directory.
withEnv :: String -> (String -> String) -> Shell a -> Shell a
withEnv key f act = do
  v <- lookupEnv key
  setEnv key $ f (maybe "" id v)
  x <- act
  maybe (return ()) (setEnv key) v
  return x

-- | Get the value of an environment variable. Returns the empty string if
--   the variable doesn't exist.
getEnv :: String -> Shell String
getEnv key = maybe "" id `fmap` lookupEnv key

-- | Run a command with elevated privileges.
sudo :: FilePath -> [String] -> String -> Shell String
sudo cmd as stdin = run "sudo" (cmd:as) stdin

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
cpdir :: FilePath -> FilePath -> Shell ()
cpdir fromdir todir = do
    dir <- isDirectory todir
    if dir
      then go fromdir todir id
      else go fromdir todir (joinPath . drop 1 . splitPath)
  where
    go from to dropFirstDir = do
      forEachDirectory_ from (\dir -> mkdir True (to </> dropFirstDir dir))
      forEachFile_ from $ \file -> do
        let file' = to </> dropFirstDir file
        assert (errOverwrite file') (not <$> isDirectory file')
        cp file file'
    errOverwrite f = "cannot overwrite directory `" ++ f
                     ++ "' with non-directory"

-- | Recursively perform an action on each subdirectory of the given directory.
--   The action will *not* be performed on the given directory itself.
forEachDirectory :: FilePath -> (FilePath -> Shell a) -> Shell [a]
forEachDirectory dir f = do
  files <- map (dir </>) <$> ls dir
  fromdirs <- filterM isDirectory files
  xs <- forM fromdirs $ \d -> do
    x <- f d
    xs <- forEachDirectory d f
    return (x:xs)
  return (concat xs)

-- | Like 'forEachDirectory', but discards its result.
forEachDirectory_ :: FilePath -> (FilePath -> Shell ()) -> Shell ()
forEachDirectory_ dir f = do
  files <- map (dir </>) <$> ls dir
  fromdirs <- filterM isDirectory files
  forM_ fromdirs $ \d -> f d >> forEachDirectory d f

-- | Perform an action on each file in the given directory.
--   This function will traverse any subdirectories of the given as well.
--   File paths are given relative to the given directory; the current working
--   directory is not affected.
forEachFile :: FilePath -> (FilePath -> Shell a) -> Shell [a]
forEachFile dir f = do
  files <- map (dir </>) <$> ls dir
  xs <- filterM isFile files >>= mapM f
  fromdirs <- filterM isDirectory files
  xss <- forM fromdirs $ \d -> do
    forEachFile d f
  return $ concat (xs:xss)

-- | Like @forEachFile@ but only performs a side effect.
forEachFile_ :: FilePath -> (FilePath -> Shell ()) -> Shell ()
forEachFile_ dir f = do
  files <- map (dir </>) <$> ls dir
  filterM isFile files >>= mapM_ f
  fromdirs <- filterM isDirectory files
  forM_ fromdirs $ \d -> do
    forEachFile d f

-- | Copy a file. Fails if the source is a directory. If the target is a
--   directory, the source file is copied into that directory using its current
--   name.
cp :: FilePath -> FilePath -> Shell ()
cp from to = do
  todir <- isDirectory to
  if todir
    then cp from (to </> takeFileName from)
    else liftIO $ Dir.copyFile from to

-- | List the contents of a directory, sans '.' and '..'.
ls :: FilePath -> Shell [FilePath]
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
withHomeDirectory :: (FilePath -> Shell a) -> Shell a
withHomeDirectory act = liftIO Dir.getHomeDirectory >>= act

-- | Perform an action with the user's home directory as the working directory.
inHomeDirectory :: Shell a -> Shell a
inHomeDirectory act = withHomeDirectory $ flip inDirectory act

-- | Do something with the given application's data directory.
withAppDirectory :: String -> (FilePath -> Shell a) -> Shell a
withAppDirectory app act = liftIO (Dir.getAppUserDataDirectory app) >>= act

-- | Do something with the given application's data directory as the working
--   directory.
inAppDirectory :: FilePath -> Shell a -> Shell a
inAppDirectory app act = withAppDirectory app $ flip inDirectory act

-- | Execute a command in the given working directory, then restore the
--   previous working directory.
inDirectory :: FilePath -> Shell a -> Shell a
inDirectory dir act = do
  curDir <- pwd
  cd dir
  x <- act
  cd curDir
  return x

-- | Does the given path lead to a directory?
isDirectory :: FilePath -> Shell Bool
isDirectory = liftIO . Dir.doesDirectoryExist

-- | Does the given path lead to a file?
isFile :: FilePath -> Shell Bool
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

-- | Read one line of input from @stdin@.
ask :: Shell String
ask = liftIO getLine

class Guard guard where
  -- | The type of the guard's return value, if it succeeds.
  type Result guard

  -- | Perform a Shell computation; if the computation succeeds but returns
  --   a false-ish value, the outer Shell computation fails with the given
  --   error message.
  assert :: String -> guard -> Shell (Result guard)

instance Guard (Maybe a) where
  type Result (Maybe a) = a
  assert _ (Just x) = return x
  assert ""  _      = fail $ "Guard failed!"
  assert desc _     = fail desc

instance Guard Bool where
  type Result Bool = ()
  assert _ True = return ()
  assert ""  _  = fail $ "Guard failed!"
  assert desc _ = fail desc

instance Guard a => Guard (Shell a) where
  type Result (Shell a) = Result a
  assert desc m = m >>= \x -> assert desc x

-- | Perform a Shell computation; if the computation succeeds but returns
--   a false-ish value, the outer Shell computation fails.
guard :: Guard g => g -> Shell (Result g)
guard = assert ""

-- | Perform the given computation if the given guard passes, otherwise do
--   nothing.
when :: Guard g => g -> Shell a -> Shell ()
when g m = (guard g >> void m) `orElse` pure ()

-- | Perform the given computation if the given guard fails, otherwise do
--   nothing.
unless :: Guard g => g -> Shell a -> Shell ()
unless g m = void (guard g) `orElse` void m
