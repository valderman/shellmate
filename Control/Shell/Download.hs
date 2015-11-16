-- | Shellmate wrapper for @download-curl@.
module Control.Shell.Download (
    URI,
    fetch, fetchBS,
    fetchFile,
    fetchTags, fetchXML, fetchFeed
  ) where
import Data.ByteString as BS (ByteString, writeFile)
import Network.Curl.Download as C
import Text.HTML.TagSoup (Tag)
import Text.XML.Light.Types (Content)
import Text.Feed.Types (Feed)
import Control.Shell

type URI = String

liftE :: IO (Either String a) -> Shell a
liftE m = do
  res <- liftIO m
  case res of
    Left e  -> fail e
    Right x -> return x

-- | Download content specified by a url using curl, returning the content
--   as a strict 'ByteString'.
fetchBS :: URI -> Shell ByteString
fetchBS = liftE . C.openURI

-- | Download content specified by a url using curl, returning the content
--   as a 'String'.
fetch :: URI -> Shell String
fetch = liftE . C.openURIString

-- | Download content specified by a url using curl, writing the content to
--   the file specified by the given 'FilePath'.
fetchFile :: FilePath -> URI -> Shell ()
fetchFile file = fetchBS >=> liftIO . BS.writeFile file

-- | Download the content as for 'fetch', but return it as a list of parsed
--   tags using the tagsoup html parser.
fetchTags :: URI -> Shell [Tag String]
fetchTags = liftE . C.openAsTags

-- | Download the content as for 'fetch', but return it as parsed XML, using
--   the xml-light parser.
fetchXML :: URI -> Shell [Content]
fetchXML = liftE . C.openAsXML

-- | Download the content as for 'fetch', but return it as as parsed RSS or
--   Atom content, using the feed library parser.
fetchFeed :: URI -> Shell Feed
fetchFeed = liftE . C.openAsFeed
