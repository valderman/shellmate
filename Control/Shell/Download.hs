{-# LANGUAGE OverloadedStrings, CPP #-}
-- | High level functions for downloading files.
module Control.Shell.Download
  ( URI
  , fetch, fetchBytes
  , fetchFile
#if DOWNLOAD_EXTRAS
  , fetchTags, fetchXML, fetchFeed
#endif
  ) where
import Data.ByteString as BS (ByteString, writeFile)
import Data.String
import Network.HTTP
import qualified Network.URI as U
#if DOWNLOAD_EXTRAS
import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Feed)
import Text.HTML.TagSoup (Tag, parseTags)
import Text.XML.Light (Content, parseXML)
#endif
import Control.Shell

-- | A Uniform Resource Locator.
type URI = String

liftE :: Show e => IO (Either e a) -> Shell a
liftE m = do
  res <- liftIO m
  case res of
    Left e  -> fail (show e)
    Right x -> return x

httpFail :: (Int, Int, Int) -> String -> Shell a
httpFail (a,b,c) reason =
  fail $ "HTTP error " ++ concat [show a, show b, show c] ++ ": " ++ reason

fetchSomething :: (IsString a, HStream a) => URI -> Shell a
fetchSomething uri = do
    u <- assert ("could not parse URI `" ++ uri ++ "'") (U.parseURI uri)
    rsp <- liftE $ simpleHTTP (req u)
    case rspCode rsp of
      (2,_,_) -> return (rspBody rsp)
      code    -> httpFail code (rspReason rsp)
  where
    req u = Request {
        rqURI = u,
        rqMethod = GET,
        rqHeaders = [],
        rqBody = ""
      }

-- | Download content specified by a url using curl, returning the content
--   as a strict 'ByteString'.
fetchBytes :: URI -> Shell ByteString
fetchBytes = fetchSomething

-- | Download content specified by a url using curl, returning the content
--   as a 'String'.
fetch :: URI -> Shell String
fetch = fetchSomething

-- | Download content specified by a url using curl, writing the content to
--   the file specified by the given 'FilePath'.
fetchFile :: FilePath -> URI -> Shell ()
fetchFile file = fetchBytes >=> liftIO . BS.writeFile file

#if DOWNLOAD_EXTRAS
-- | Download the content as for 'fetch', but return it as a list of parsed
--   tags using the tagsoup html parser.
fetchTags :: URI -> Shell [Tag String]
fetchTags = fmap parseTags . fetch

-- | Download the content as for 'fetch', but return it as parsed XML, using
--   the xml-light parser.
fetchXML :: URI -> Shell [Content]
fetchXML = fmap parseXML . fetch

-- | Download the content as for 'fetch', but return it as as parsed RSS or
--   Atom content, using the feed library parser.
fetchFeed :: URI -> Shell Feed
fetchFeed uri = do
  str <- fetch uri
  assert ("could not parse feed from `" ++ uri ++ "'") (parseFeedString str)
#endif
