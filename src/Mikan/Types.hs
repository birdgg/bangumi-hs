module Mikan.Types where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Types.HTML (FromHTML (..))
import Data.Types.Rss (FromRss (..), Rss)
import GHC.Generics (Generic)
import Network.HTTP.Types.URL (URL)
import Text.HTML.Scalpel.Core (attr, chroot, chroots, hasClass, text, (@:))

data RssItem = RssItem {rssTitle :: T.Text, rssTorrent :: URL}
  deriving (Show, Generic)

type MikanRss = Rss RssItem

instance FromRss RssItem where
  fromRss = do
    t <- text "title"
    torr <- attr "url" "enclosure"
    pure RssItem{rssTitle = t, rssTorrent = torr}

instance ToJSON RssItem

data Bangumi = Bangumi
  { bangumiTitle :: T.Text
  , bangumiId :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Bangumi

instance FromHTML Bangumi where
  fromHTML =
    Bangumi
      <$> text ("div" @: [hasClass "an-text"])
      <*> (extractId <$> attr "href" "a")

newtype BangumiList = BangumiList [Bangumi] deriving (Generic, Show)

instance ToJSON BangumiList

instance FromHTML BangumiList where
  fromHTML =
    BangumiList
      <$> chroot ("ul" @: [hasClass "an-ul"]) (chroots "li" fromHTML)

{- | Extract numeric id from URLs like "/Home/Bangumi/3342".
  Returns Nothing if no trailing integer segment is found.
-}
extractId :: T.Text -> Maybe Int
extractId href =
  case TR.decimal lastSeg of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing
 where
  lastSeg = last (T.splitOn "/" href)
