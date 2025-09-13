module Moe.Bangumi.Bangumi where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics

data Source = WebRip | BDRip deriving (Show)

-- | Essential information about a bangumi.
data MiniBangumi = MiniBangumi
  { title :: T.Text
  , titleJp :: T.Text
  , season :: T.Text
  , summary :: T.Text
  , post :: T.Text
  , eps :: Int
  }
  deriving (Show, Generic)

instance ToJSON Bangumi
