module Mikan.Types where

import Data.Aeson
import Data.Text qualified as T
import Network.Types.HTTP.URL
import Rss.Types
import Servant.API.ContentTypes.RSS
import Text.HTML.Scalpel.Core qualified as Scalpel
import GHC.Generics (Generic)


data MikanRssItem = MikanRssItem {title :: T.Text, torrent :: URL} deriving (Show, Generic)

type MikanRss = Rss MikanRssItem

instance FromRss MikanRssItem where
    fromRss = do
        t <- Scalpel.text "title"
        torr <- Scalpel.attr "url" "enclosure"
        pure MikanRssItem{title = t, torrent = torr}

instance ToJSON MikanRssItem
instance FromJSON MikanRssItem
