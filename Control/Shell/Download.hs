-- | Shellmate wrapper for @download-curl@.
module Control.Shell.Download (
    fetch, fetchBS,
    fetchTags, fetchXML, fetchFeed
  ) where
import Network.Curl.Download as C
import Data.ByteString (ByteString)
import Control.Shell
import Text.HTML.TagSoup (Tag)
import Text.XML.Light.Types (Content)
import Text.Feed.Types (Feed)

liftE :: IO (Either String a) -> Shell a
liftE m = do
  res <- liftIO m
  case res of
    Left e  -> fail e
    Right x -> return x

-- | Download content specified by a url using curl, returning the content
--   as a strict 'ByteString'.
fetchBS :: String -> Shell ByteString
fetchBS = liftE . C.openURI

-- | Download content specified by a url using curl, returning the content
--   as a 'String'.
fetch :: String -> Shell String
fetch = liftE . C.openURIString

-- | Download the content as for 'fetch', but return it as a list of parsed
--   tags using the tagsoup html parser.
fetchTags :: String -> Shell [Tag String]
fetchTags = liftE . C.openAsTags

-- | Download the content as for 'fetch', but return it as parsed XML, using
--   the xml-light parser.
fetchXML :: String -> Shell [Content]
fetchXML = liftE . C.openAsXML

-- | Download the content as for 'fetch', but return it as as parsed RSS or
--   Atom content, using the feed library parser.
fetchFeed :: String -> Shell Feed
fetchFeed = liftE . C.openAsFeed
