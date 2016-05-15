-- | Writing Shell programs using 'Handle's.
module Control.Shell.Handle (
    hPutStr, hPutStrLn,
    hGetLine, hGetContents,
    hGetBytes, hPutBytes, hGetByteLine, hGetByteContents,
    hFlush, hClose
  ) where
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Control.Shell.Base

-- | Write a string to a handle.
hPutStr :: IO.Handle -> String -> Shell ()
hPutStr h s = unsafeLiftIO $ IO.hPutStr h s

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: IO.Handle -> String -> Shell ()
hPutStrLn h s = unsafeLiftIO $ IO.hPutStrLn h s

-- | Close a handle.
hClose :: IO.Handle -> Shell ()
hClose = unsafeLiftIO . IO.hClose

-- | Flush a handle.
hFlush :: IO.Handle -> Shell ()
hFlush = unsafeLiftIO . IO.hFlush

-- | Read a line of input from a handle.
hGetLine :: IO.Handle -> Shell String
hGetLine = unsafeLiftIO . IO.hGetLine

-- | Lazily read all remaining input from a handle.
hGetContents :: IO.Handle -> Shell String
hGetContents = unsafeLiftIO . IO.hGetContents

-- | Read @n@ bytes from a handle.
hGetBytes :: IO.Handle -> Int -> Shell BS.ByteString
hGetBytes h = unsafeLiftIO . BS.hGet h

-- | Read a line of input from a handle and return it as a 'BS.ByteString'.
hGetByteLine :: IO.Handle -> Shell BS.ByteString
hGetByteLine = unsafeLiftIO . BS.hGetLine

-- | Read all remaining input from a handle and return it as a 'BS.ByteString'.
hGetByteContents :: IO.Handle -> Shell BS.ByteString
hGetByteContents = unsafeLiftIO . BS.hGetContents

-- | Write a 'BS.ByteString' to a handle. Newline is not appended.
hPutBytes :: IO.Handle -> BS.ByteString -> Shell ()
hPutBytes h = unsafeLiftIO . BS.hPutStr h
