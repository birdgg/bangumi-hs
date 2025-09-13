module Moe.Model.Torrent where

import GHC.Generics (Generic)
import Network.HTTP.Types (URL)

data Torrent = Torrent
  { title :: T.Text
  , link :: URL
  }
  deriving (Show, Eq, Generic)
