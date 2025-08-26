module Servant.API.ContentTypes.RSS  where

import Data.ByteString qualified as B
import Data.Either.Extra
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Media qualified as M
import Servant.API
import Text.HTML.Scalpel.Core
import Text.HTML.TagSoup qualified as TagSoup
import Control.Monad
import Data.List.NonEmpty
import Data.Bifunctor

data RSS

instance Accept RSS where
    contentTypes _ = "application" M.// "xml" M./: ("charset", "utf-8") :| ["application" M.// "xml"]

class FromRss a where
    fromRss :: Scraper T.Text a

instance (FromRss a) => MimeUnrender RSS a where
    mimeUnrender _ =
        first show
            . T.decodeUtf8'
            . B.toStrict
            >=> maybeToEither "RSS parse failed"
            . scrapeRss
      where
        scrapeRss :: T.Text -> Maybe a
        scrapeRss = scrape fromRss . TagSoup.parseTags
