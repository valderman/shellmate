-- | Working with directories.
module Control.Shell.Directory where
import qualified System.Directory as Dir
import Control.Monad
import Control.Shell.Base
import Control.Shell.Control
import Control.Shell.File

-- | Get the current working directory.
pwd :: Shell FilePath
pwd = envWorkDir <$> getEnv

-- | Recursively copy a directory. If the target is a directory that already
--   exists, the source directory is copied into that directory using its
--   current name.
cpdir :: FilePath -> FilePath -> Shell ()
cpdir f t = do
    e <- getEnv
    let fromdir = absPath e f
        todir = absPath e t
    assert ("`" ++ fromdir ++ "' is not a directory") (isDirectory fromdir)
    exists <- isDirectory todir
    if exists
      then do
        mkdir True (todir </> takeFileName fromdir)
        go fromdir (todir </> takeFileName fromdir)
      else mkdir True todir >> go fromdir todir
  where
    go from to = do
      forEachDirectory_ from (\dir -> mkdir True (to </> dir))
      forEachFile_ from $ \file -> do
        let file' = to </> file
        assert (errOverwrite file') (not <$> isDirectory file')
        cp (from </> file) file'
    errOverwrite d = "cannot overwrite directory `" ++ d
                     ++ "' with non-directory"

-- | Recursively perform an action on each subdirectory of the given directory.
--   The path passed to the callback is relative to the given directory.
--   The action will *not* be performed on the given directory itself.
forEachDirectory :: FilePath -> (FilePath -> Shell a) -> Shell [a]
forEachDirectory r f = do
    e <- getEnv
    go (absPath e $ if null r then "." else r) ""
  where
    go dir subdir = do
      let dir' = dir </> subdir
      files <- ls dir'
      fromdirs <- filterM (\d -> isDirectory (dir' </> d)) files
      xs <- forM fromdirs $ \d -> do
        let d' = subdir </> d
        x <- f d'
        (x:) <$> go dir d'
      return (concat xs)

-- | Like 'forEachDirectory', but discards its result.
forEachDirectory_ :: FilePath -> (FilePath -> Shell ()) -> Shell ()
forEachDirectory_ r f = do
    e <- getEnv
    go (absPath e $ if null r then "." else r) ""
  where
    go dir subdir = do
      let dir' = dir </> subdir
      files <- ls dir'
      fromdirs <- filterM (\d -> isDirectory (dir' </> d)) files
      forM_ fromdirs $ \d -> let d' = subdir </> d in f d' >> go dir d'

-- | Perform an action on each file in the given directory.
--   This function will traverse any subdirectories of the given as well.
--   File paths are given relative to the given directory; the current working
--   directory is not affected.
forEachFile :: FilePath -> (FilePath -> Shell a) -> Shell [a]
forEachFile r f = do
    e <- getEnv
    go (absPath e $ if null r then "." else r) ""
  where
    go dir subdir = do
      let dir' = dir </> subdir
      files <- ls dir'
      
      -- For each file in this directory...
      onlyfiles <- filterM (\fl -> isFile (dir' </> fl)) files
      xs <- mapM (\x -> f (subdir </> x)) onlyfiles

      -- For each subdirectory...
      fromdirs <- filterM (\fl -> isDirectory (dir' </> fl)) files
      xss <- forM fromdirs $ \d -> do
        go dir (subdir </> d)
      return $ concat (xs:xss)

-- | Like @forEachFile@ but only performs a side effect.
forEachFile_ :: FilePath -> (FilePath -> Shell ()) -> Shell ()
forEachFile_ r f = do
    e <- getEnv
    go (absPath e $ if null r then "." else r) ""
  where
    go dir subdir = do
      let dir' = dir </> subdir
      files <- ls dir'
      filterM (\fl -> isFile (dir' </> fl)) files >>= mapM_ (f . (subdir </>))
      fromdirs <- filterM (\fl -> isDirectory (dir' </> fl)) files
      forM_ fromdirs $ \d -> go dir (subdir </> d)

-- | List the contents of a directory, sans @.@ and @..@.
ls :: FilePath -> Shell [FilePath]
ls dir = do
  e <- getEnv
  contents <- unsafeLiftIO $ Dir.getDirectoryContents (absPath e dir)
  return [f | f <- contents, f /= ".", f /= ".."]

-- | Create a directory. Optionally create any required missing directories as
--   well.
mkdir :: Bool -> FilePath -> Shell ()
mkdir True dir = do
  e <- getEnv
  unsafeLiftIO $ Dir.createDirectoryIfMissing True (absPath e dir)
mkdir _    dir = do
  e <- getEnv
  unsafeLiftIO $ Dir.createDirectory (absPath e dir)

-- | Recursively remove a directory. Follows symlinks, so be careful.
rmdir :: FilePath -> Shell ()
rmdir dir = do
  e <- getEnv
  unsafeLiftIO $ do
    let p = absPath e dir
    makeWritableRecursive p
    Dir.removeDirectoryRecursive p

-- Needed to recursively remove directories with read-only files on Windows.
-- Thanks to Ruud @ https://stackoverflow.com/questions/38926895/haskell-removedirectoryrecursive-permission-denied-on-windows
makeWritableRecursive :: FilePath -> IO ()
makeWritableRecursive path = do
  permissions <- Dir.getPermissions path
  Dir.setPermissions path (Dir.setOwnerWritable True permissions)
  isDirectory <- Dir.doesDirectoryExist path
  Control.Monad.when isDirectory $ do
    contents <- Dir.getDirectoryContents path
    forM_ [path </> item | item <- contents, item /= "." && item /= ".."] makeWritableRecursive

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
  env <- getEnv
  inEnv (env {envWorkDir = absPath env dir}) act

-- | Does the given path lead to a directory?
isDirectory :: FilePath -> Shell Bool
isDirectory dir = do
  e <- getEnv
  unsafeLiftIO $ Dir.doesDirectoryExist (absPath e dir)

-- | Does the given path lead to a file?
isFile :: FilePath -> Shell Bool
isFile f = do
  e <- getEnv
  unsafeLiftIO $ Dir.doesFileExist (absPath e f)
