module Moe.Model where

import Data.Aeson

data Bangumi = Bangumi
  { title :: Text
  }
  deriving (Show, Generic)

instance ToJSON Bangumi
