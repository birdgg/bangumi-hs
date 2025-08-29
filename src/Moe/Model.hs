module Moe.Model where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Bangumi = Bangumi
  { title :: Text
  }
  deriving (Show, Generic)

instance ToJSON Bangumi
