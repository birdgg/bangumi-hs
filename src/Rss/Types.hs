module Rss.Types where

import Network.HTTP.Types.URL
import RIO
import RIO.Text qualified as T
import Servant.API.ContentTypes.RSS
import Text.HTML.Scalpel.Core qualified as Scalpel

data Rss a = Rss {title :: T.Text, link :: URL, description :: T.Text, items :: [a]} deriving (Show)

instance (FromRss a) => FromRss (Rss a) where
    fromRss = Scalpel.chroot "channel" $ do
        rssTitle <- Scalpel.text "title"
        rssLink <- Scalpel.text "link"
        rssDescription <- Scalpel.text "description"
        rssItems <- Scalpel.chroots "item" fromRss
        pure Rss{title = rssTitle, link = rssLink, description = rssDescription, items = rssItems}
