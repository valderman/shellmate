-- | Temporary files and directories.
module Control.Shell.Temp
  ( withTempDirectory, withCustomTempDirectory
  , withTempFile, withCustomTempFile
  ) where
import Control.Shell.Base
import qualified System.IO.Temp as Temp
import qualified System.IO as IO

-- | Create a temp directory in the standard system temp directory, do
--   something with it, then remove it.
withTempDirectory :: (FilePath -> Shell a) -> Shell a
withTempDirectory act = joinResult $ do
  env <- getEnv
  takeEnvLock
  oldenv <- setShellEnv env
  unsafeLiftIO $ do
    Temp.withSystemTempDirectory "shellmate" $ \fp -> runSh env $ do
      setShellEnv oldenv
      releaseEnvLock
      act fp

-- | Create a temp directory in given directory, do something with it, then
--   remove it.
withCustomTempDirectory :: FilePath -> (FilePath -> Shell a) -> Shell a
withCustomTempDirectory dir act = joinResult $ do
  env <- getEnv
  takeEnvLock
  oldenv <- setShellEnv env
  unsafeLiftIO $ do
    Temp.withTempDirectory dir "shellmate" $ \fp -> runSh env $ do
      setShellEnv oldenv
      releaseEnvLock
      act fp

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withTempFile :: FileMode -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withTempFile fm act = joinResult $ do
  env <- getEnv
  takeEnvLock
  oldenv <- setShellEnv env
  unsafeLiftIO $ do
    Temp.withSystemTempFile "shellmate" $ \fp h -> runSh env $ do
      setShellEnv oldenv
      releaseEnvLock
      unsafeLiftIO $ IO.hSetBinaryMode h (fm == BinaryMode)
      act fp h

-- | Create a temp file in the standard system temp directory, do something
--   with it, then remove it.
withCustomTempFile :: FileMode -> FilePath -> (FilePath -> IO.Handle -> Shell a) -> Shell a
withCustomTempFile fm dir act = joinResult $ do
  env <- getEnv
  takeEnvLock
  oldenv <- setShellEnv env
  unsafeLiftIO $ do
    Temp.withTempFile dir "shellmate" $ \fp h -> runSh env $ do
      setShellEnv oldenv
      releaseEnvLock
      unsafeLiftIO $ IO.hSetBinaryMode h (fm == BinaryMode)
      act fp h
