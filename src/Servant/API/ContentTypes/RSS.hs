module Servant.API.ContentTypes.RSS (RSS, FromRss (..)) where

import Data.Either.Extra
import Network.HTTP.Media qualified as M
import RIO
import RIO.ByteString qualified as B
import RIO.Text qualified as T

-- import Rss.Types
import Servant.API
import Text.HTML.Scalpel.Core
import Text.HTML.TagSoup qualified as TagSoup

data RSS

instance Accept RSS where
    contentTypes _ = "application" M.// "xml" M./: ("charset", "utf-8") :| ["application" M.// "xml"]

class FromRss a where
    fromRss :: Scraper T.Text a

instance (FromRss a) => MimeUnrender RSS a where
    mimeUnrender _ bs = do
        t <- first show $ T.decodeUtf8' $ B.toStrict bs
        maybeToEither "Parse failed" $ scrape fromRss $ TagSoup.parseTags t
