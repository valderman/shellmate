-- | Working with files.
module Control.Shell.File
  ( rm, mv, cp
  , input, output
  , IO.IOMode (..)
  , withFile, withBinaryFile
  , openFile, openBinaryFile
  ) where
import qualified System.Directory as Dir
import qualified System.IO as IO
import Control.Shell.Base

-- | Lazily read a file.
input :: FilePath -> Shell String
input f = do
  e <- getEnv
  unsafeLiftIO $ readFile (absPath e f)

-- | Lazily write a file.
output :: FilePath -> String -> Shell ()
output f s = do
  e <- getEnv
  unsafeLiftIO $ writeFile (absPath e f) s

-- | Remove a file.
rm :: FilePath -> Shell ()
rm dir = do
  e <- getEnv
  unsafeLiftIO $ Dir.removeFile (absPath e dir)

-- | Rename a file or directory. If the target is a directory, then the source
--   will be moved into that directory.
mv :: FilePath -> FilePath -> Shell ()
mv from to = do
  e <- getEnv
  unsafeLiftIO $ do
    targetIsDir <- Dir.doesDirectoryExist (absPath e to)
    sourceIsDir <- Dir.doesDirectoryExist (absPath e from)
    if sourceIsDir
      then Dir.renameDirectory (absPath e from) (targetPath targetIsDir e)
      else Dir.renameFile (absPath e from) (targetPath targetIsDir e)
  where
    targetPath True e  = absPath e to </> takeBaseName from
    targetPath False e = absPath e to

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

-- | Perform a computation over a file.
withFile :: FilePath -> IO.IOMode -> (IO.Handle -> Shell a) -> Shell a
withFile file mode f = joinResult $ do
  e <- getEnv
  unsafeLiftIO (IO.withFile (absPath e file) mode (shell . f))

-- | Perform a computation over a binary file.
withBinaryFile :: FilePath -> IO.IOMode -> (IO.Handle -> Shell a) -> Shell a
withBinaryFile file mode f = joinResult $ do
  e <- getEnv
  unsafeLiftIO (IO.withBinaryFile (absPath e file) mode (shell . f))

-- | Open a file, returning a handle to it.
openFile :: FilePath -> IO.IOMode -> Shell IO.Handle
openFile file mode = do
  e <- getEnv
  unsafeLiftIO $ IO.openFile (absPath e file) mode

-- | Open a file in binary mode, returning a handle to it.
openBinaryFile :: FilePath -> IO.IOMode -> Shell IO.Handle
openBinaryFile file mode = do
  e <- getEnv
  unsafeLiftIO $ IO.openBinaryFile (absPath e file) mode
