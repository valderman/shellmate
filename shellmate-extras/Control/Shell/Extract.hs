-- | Extract various archive files in a consistent manner.
{-# LANGUAGE OverloadedStrings #-}
module Control.Shell.Extract
  ( -- * Extracting archives
    extract, extractWith, supportedExtensions
    -- * Extraction options
  , ExtractOptions, separateDirectory, extractVerbose, removeArchive
  , defaultExtractOptions
  ) where
import Control.Shell
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.Mime
import System.IO.Unsafe

data ExtractOptions = ExtractOptions
  { -- | Extract archive into a separate directory. The anme of the directory
    --   will be the base name of the archive.
    --   If the archive contains a single directory on the top-level, the
    --   contents of that directory will be moved into the outer directory.
    --   This ensures that tarbombs and non-tarbombs are treated consistently.
    --
    --   Default: @True@
    separateDirectory :: Bool

    -- | Print verbose extraction output? If False, extraction will be silent.
    --
    --   Default: @False@
  , extractVerbose    :: Bool

    -- | Remove the archive after extraction?
    --
    --   Default: @False@
  , removeArchive     :: Bool
  }

-- | Default extraction options. See 'ExtractOptions' for defaults.
defaultExtractOptions :: ExtractOptions
defaultExtractOptions = ExtractOptions
  { separateDirectory = True
  , extractVerbose    = False
  , removeArchive     = False
  }

-- | The list of supported archive format file extensions.
--   On a typical Linux/OSX/Cygwin/MinGW system, .tar.gz, .tar.bz2, .tar.xz,
--   .tbz2, .tgz, .bz2, .gz and .xz should all be supported. If the appropriate
--   programs are installed, .zip, .rar and .7z should also be supported.
supportedExtensions :: [String]
supportedExtensions = unsafePerformIO $ do
  env <- shell_ $ getShellEnv
  res <- runSh (env {envStdErr = envStdOut env}) $ do
    tar <- (capture (run "tar" ["-?"]) >> pure tarExts) `orElse` pure []
    z7 <- (capture (run "7z" []) >> pure [".7z"]) `orElse` pure []
    rar <- (capture (run "unrar" []) >> pure [".rar"]) `orElse` pure []
    zip <- (capture (run "unzip" []) >> pure [".zip"]) `orElse` pure []
    xz <- (capture (run "xz" ["--help"]) >> pure [".xz"]) `orElse` pure []
    bz2 <- (capture (run "bunzip2" ["--help"]) >> pure [".bz2"]) `orElse` pure []
    gz <- (capture (run "gunzip" ["--help"]) >> pure [".gz"]) `orElse` pure []
    pure $ concat [tar, z7, rar, zip, xz, bz2, gz]
  case res of
    Left _   -> pure []
    Right xs -> pure xs
  where
     tarExts = [".tar.gz", ".tar.bz2", ".tar.xz", ".tbz2", ".tgz"]

-- | Extract an archive with the default options. See 'ExtractOptions'
--   for details.
extract :: FilePath -> Shell ()
extract = extractWith defaultExtractOptions

-- | Extract an archive with the given extraction options.
extractWith :: ExtractOptions -> FilePath -> Shell ()
extractWith opts file = do
    archivedir <- pwd
    let archive = archivedir </> file
    mkdir True outputDir
    case extractCmd archive of
      Nothing          -> mimeFail
      Just (cmd, args) -> inDirectory outputDir $ do
        maybeCapture $ run cmd (args ++ [archive] ++ verboseArgs)
        moveOutputToWorkDir archive
        when (separateDirectory opts) $ mergeOneLevelRoot `orElse` pure ()
        when (removeArchive opts) $ rm archive
  where
    outputDir
      | separateDirectory opts = takeBasestName file
      | otherwise              = "."
    verboseArgs
      | extractVerbose opts    = ["-v"]
      | otherwise              = []
    maybeCapture
      | extractVerbose opts    = id
      | otherwise              = void . capture
    mimeFail = fail $ concat
      [ "mime type does not seem to be an archive: "
      , BS.unpack $ defaultMimeLookup (T.pack file)
      ]

-- | If the current working directory contains a single directory, move all
--   contents of that directory into the working directory, then remove the
--   directory.
mergeOneLevelRoot :: Shell ()
mergeOneLevelRoot = do
  [dir] <- ls "."
  guard $ isDirectory dir
  inCustomTempDirectory "." $ do
    mv (".." </> dir) "."
    files <- ls dir
    mapM_ (\f -> mv (dir </> f) "..") files

-- | Command + arguments to extract an archive file.
--   Arguments must be followed immediately by the archive file name.
extractCmd :: FilePath -> Maybe (FilePath, [String])
extractCmd f =
  case defaultMimeLookup (T.pack f) of
    "application/x-7z-compressed"       -> Just ("7z", ["x"])
    "application/zip"                   -> Just ("unzip", [])
    "application/x-rar-compressed"      -> Just ("unrar", ["x"])
    "application/x-tar"                 -> Just ("tar", ["-xf"])
    "application/x-tgz"                 -> Just ("tar", ["-xzf"])
    "application/x-bzip-compressed-tar" -> Just ("tar", ["-xjf"])
    "application/x-bzip"                -> Just ("bunzip2", ["-k"])
    "application/x-gzip"                -> Just ("gunzip", ["-k"])
    "application/x-xz"
      | "application/x-tar" <- defaultMimeLookup (T.pack $ dropExtension f)
                                        -> Just ("tar", ["-xJf"])
      | otherwise                       -> Just ("xz", ["-dk"])
    _                                   -> Nothing

-- | Move the extracted file of the given archive into the current working
--   directory, if the archive is .gz, .bz2 or .xz. Workaround for bunzip,
--   gunzip2 and xz insisting on unpacking files to the same directory as the
--   archive instead of to the current working directory.
moveOutputToWorkDir :: FilePath -> Shell ()
moveOutputToWorkDir f = when needsWorkaround $ mv f' (takeFileName f')
  where
    f' = dropExtension f
    needsWorkaround =
      case defaultMimeLookup (T.pack f) of
        "application/x-bzip" -> True
        "application/x-gzip" -> True
        "application/x-xz"   ->
          "application/x-tar" /= defaultMimeLookup (T.pack f')
        _                    -> False

-- | Iterate 'takeBaseName' until all extensions are gone.
takeBasestName :: FilePath -> FilePath
takeBasestName f
  | f == f'   = f
  | otherwise = takeBasestName f'
  where f' = takeBaseName f 
