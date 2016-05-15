-- | Working with files.
module Control.Shell.File
  ( rm, mv, cp
  ) where
import qualified System.Directory as Dir
import Control.Shell.Base

-- | Remove a file.
rm :: FilePath -> Shell ()
rm dir = do
  e <- getEnv
  unsafeLiftIO $ Dir.removeFile (absPath e dir)

-- | Rename a file.
mv :: FilePath -> FilePath -> Shell ()
mv from to = do
  e <- getEnv
  unsafeLiftIO $ Dir.renameFile (absPath e from) (absPath e to)

-- | Copy a file. Fails if the source is a directory. If the target is a
--   directory, the source file is copied into that directory using its current
--   name.
cp :: FilePath -> FilePath -> Shell ()
cp f t = do
  e <- getEnv
  let (from, to) = (absPath e f, absPath e t)
  todir <- unsafeLiftIO $ Dir.doesDirectoryExist to
  if todir
    then cp from (to </> takeFileName from)
    else liftIO $ Dir.copyFile from to
