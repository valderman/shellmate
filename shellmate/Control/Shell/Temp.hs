-- | Temporary files and directories.
module Control.Shell.Temp
  ( withTempDirectory, withCustomTempDirectory
  , withTempFile, withCustomTempFile
  ) where
import Control.Shell.Base
import Control.Shell.Directory (rmdir, isDirectory)
import Control.Shell.Control (when)
import qualified System.IO.Temp as Temp
import qualified System.IO as IO

-- | Create a temp directory in the standard system temp directory, do
--   something with it, then remove it.
withTempDirectory :: (FilePath -> Shell a) -> Shell a
withTempDirectory act = joinResult $ do
  env <- getEnv
  takeEnvLock
  unsafeLiftIO $ do
    oldenv <- setShellEnv env
    Temp.withSystemTempDirectory "shellmate" $ \fp -> do
      setShellEnv oldenv
      runSh env $ do
        releaseEnvLock
        act fp

-- | Create a temp directory in given directory, do something with it, then
--   remove it.
withCustomTempDirectory :: FilePath -> (FilePath -> Shell a) -> Shell a
withCustomTempDirectory dir act = joinResult $ do
  env <- getEnv
  takeEnvLock
  unsafeLiftIO $ do
    oldenv <- setShellEnv env
    Temp.withTempDirectory dir "shellmate" $ \fp -> do
      setShellEnv oldenv
      runSh env $ do
        releaseEnvLock
        x <- act fp
        when (isDirectory fp) $ rmdir fp
        return x

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withTempFile :: FileMode -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withTempFile fm act = joinResult $ do
  env <- getEnv
  takeEnvLock
  unsafeLiftIO $ do
    oldenv <- setShellEnv env
    Temp.withSystemTempFile "shellmate" $ \fp h -> do
      setShellEnv oldenv
      runSh env $ do
        releaseEnvLock
        unsafeLiftIO $ IO.hSetBinaryMode h (fm == BinaryMode)
        act fp h

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withCustomTempFile :: FileMode -> FilePath -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withCustomTempFile fm dir act = joinResult $ do
  env <- getEnv
  takeEnvLock
  unsafeLiftIO $ do
    oldenv <- setShellEnv env
    Temp.withTempFile dir "shellmate" $ \fp h -> do
      setShellEnv oldenv
      runSh env $ do
        releaseEnvLock
        unsafeLiftIO $ IO.hSetBinaryMode h (fm == BinaryMode)
        act fp h
