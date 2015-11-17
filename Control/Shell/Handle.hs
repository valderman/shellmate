-- | Writing Shell programs using 'Handle's.
module Control.Shell.Handle (
    IO.Handle, IO.IOMode (..),
    IO.stdin, IO.stdout, IO.stderr,

    hPutStr, hPutStrLn,
    hGetLine, hGetContents,

    hGetBytes, hPutBytes, hGetByteLine, hGetByteContents,

    hFlush, hClose, withFile, withBinaryFile, openFile, openBinaryFile
  ) where
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Control.Shell.Internal

-- | Write a string to a handle.
hPutStr :: IO.Handle -> String -> Shell ()
hPutStr h s = liftIO $ IO.hPutStr h s

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: IO.Handle -> String -> Shell ()
hPutStrLn h s = liftIO $ IO.hPutStrLn h s

-- | Close a handle.
hClose :: IO.Handle -> Shell ()
hClose = liftIO . IO.hClose

-- | Flush a handle.
hFlush :: IO.Handle -> Shell ()
hFlush = liftIO . IO.hFlush

-- | Read a line of input from a handle.
hGetLine :: IO.Handle -> Shell String
hGetLine = liftIO . IO.hGetLine

-- | Lazily read all remaining input from a handle.
hGetContents :: IO.Handle -> Shell String
hGetContents = liftIO . IO.hGetContents

-- | Read @n@ bytes from a handle.
hGetBytes :: IO.Handle -> Int -> Shell BS.ByteString
hGetBytes h = liftIO . BS.hGet h

-- | Read a line of input from a handle and return it as a 'BS.ByteString'.
hGetByteLine :: IO.Handle -> Shell BS.ByteString
hGetByteLine = liftIO . BS.hGetLine

-- | Read all remaining input from a handle and return it as a 'BS.ByteString'.
hGetByteContents :: IO.Handle -> Shell BS.ByteString
hGetByteContents = liftIO . BS.hGetContents

-- | Write a 'BS.ByteString' to a handle. Newline is not appended.
hPutBytes :: IO.Handle -> BS.ByteString -> Shell ()
hPutBytes h = liftIO . BS.hPutStr h

-- | Perform a computation over a file.
withFile :: FilePath -> IO.IOMode -> (IO.Handle -> Shell a) -> Shell a
withFile file mode f =
  liftIO (IO.withFile file mode (shell . f)) >>= liftResult

-- | Perform a computation over a binary file.
withBinaryFile :: FilePath -> IO.IOMode -> (IO.Handle -> Shell a) -> Shell a
withBinaryFile file mode f =
  liftIO (IO.withBinaryFile file mode (shell . f)) >>= liftResult

-- | Open a file, returning a handle to it.
openFile :: FilePath -> IO.IOMode -> Shell IO.Handle
openFile file = liftIO . IO.openFile file

-- | Open a file in binary mode, returning a handle to it.
openBinaryFile :: FilePath -> IO.IOMode -> Shell IO.Handle
openBinaryFile file = liftIO . IO.openBinaryFile file

liftResult :: Either ExitReason a -> Shell a
liftResult (Right x)            = return x
liftResult (Left Success)       = exit
liftResult (Left (Failure err)) = fail err
