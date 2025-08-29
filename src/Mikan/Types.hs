module Mikan.Types where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics
import Network.HTTP.Types.URL
import Rss.Types
import Text.HTML.Scalpel.Core qualified as Scalpel

data MikanRssItem = MikanRssItem {title :: T.Text, torrent :: URL}
  deriving (Show, Generic)

type MikanRss = Rss MikanRssItem

instance FromRss MikanRssItem where
  fromRss = do
    t <- Scalpel.text "title"
    torr <- Scalpel.attr "url" "enclosure"
    pure MikanRssItem{title = t, torrent = torr}

instance ToJSON MikanRssItem
instance FromJSON MikanRssItem
