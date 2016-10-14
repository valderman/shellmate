{-# LANGUAGE OverloadedStrings #-}
-- | High level functions for downloading files.
module Control.Shell.Download
  ( URI
  , fetch, fetchBytes
  , fetchFile
  , fetchTags, fetchXML, fetchFeed
  ) where
import Data.ByteString as BS (ByteString, writeFile)
import Data.ByteString.UTF8 as BS
import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 as LBS
import Data.String
import Network.HTTP.Simple
import Network.HTTP.Types
import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Feed)
import Text.HTML.TagSoup (Tag, parseTags)
import Text.XML.Light (Content, parseXML)
import Control.Shell

-- | A Uniform Resource Locator.
type URI = String

liftE :: Show e => IO (Either e a) -> Shell a
liftE m = do
  res <- liftIO m
  case res of
    Left e  -> fail (show e)
    Right x -> return x

httpFail :: Int -> BS.ByteString -> Shell a
httpFail code reason =
  fail $ "HTTP error " ++ show code ++ ": " ++ BS.toString reason

fetchSomething :: URI -> Shell LBS.ByteString
fetchSomething uri = do
  req <- assert ("could not parse URI `" ++ uri ++ "'") $ do
    try $ liftIO $ parseRequest uri
  rsp <- httpLBS req
  case getResponseStatus rsp of
    (Status 200 _)       -> return (getResponseBody rsp)
    (Status code reason) -> httpFail code reason

-- | Download content specified by a URL, returning the content
--   as a strict 'ByteString'.
fetchBytes :: URI -> Shell BS.ByteString
fetchBytes = fmap LBS.toStrict . fetchSomething

-- | Download content specified by a URL, returning the content
--   as a 'String'. The content is interpreted as UTF8.
fetch :: URI -> Shell String
fetch = fmap LBS.toString . fetchSomething

-- | Download content specified by a URL, writing the content to
--   the file specified by the given 'FilePath'.
fetchFile :: FilePath -> URI -> Shell ()
fetchFile file = fetchSomething >=> liftIO . LBS.writeFile file

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
  str <- LBS.toString <$> fetchSomething uri
  assert ("could not parse feed from `" ++ uri ++ "'") (parseFeedString str)
