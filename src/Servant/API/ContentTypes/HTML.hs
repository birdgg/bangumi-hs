module Servant.API.ContentTypes.HTML where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Either.Extra (maybeToEither)
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding qualified as T
import Data.Types.HTML (FromHTML, fromHTML)
import Network.HTTP.Media qualified as M
import Servant.API.ContentTypes (Accept (..), MimeUnrender (..))
import Text.HTML.Scalpel.Core (scrape)
import Text.HTML.TagSoup qualified as TagSoup

data HTML

instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8")
      NE.:| ["text" M.// "html"]

instance (FromHTML a) => MimeUnrender HTML a where
  mimeUnrender _ =
    first show
      . T.decodeUtf8'
      . B.toStrict
      >=> maybeToEither "scrape failed"
        . scrape fromHTML
        . TagSoup.parseTags
