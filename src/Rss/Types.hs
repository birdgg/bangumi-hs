module Rss.Types where

import Data.Text qualified as T
import Network.HTTP.Types.URL
import Text.HTML.Scalpel.Core

data Rss a = Rss {title :: T.Text, link :: URL, description :: T.Text, items :: [a]} deriving (Show)

class FromRss a where
  fromRss :: Scraper T.Text a

instance (FromRss a) => FromRss (Rss a) where
  fromRss = chroot "channel" $ do
    rssTitle <- text "title"
    rssLink <- text "link"
    rssDescription <- text "description"
    rssItems <- chroots "item" fromRss
    pure Rss{title = rssTitle, link = rssLink, description = rssDescription, items = rssItems}
