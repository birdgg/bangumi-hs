module BgmTV.Types where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Text qualified as T
import GHC.Generics

-- | Pagination wrapper for API responses
newtype Pagination a = Pagination
  { pData :: [a]
  }
  deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Pagination a) where
  parseJSON = withObject "Pagination" $ \o -> do
    dat <- o .: "data"
    return $ Pagination dat

-- | Image URLs in different sizes
data Images = Images
  { small :: T.Text
  , grid :: T.Text
  , large :: T.Text
  , medium :: T.Text
  , common :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Images
instance ToJSON Images

-- | Subject information from BGM.TV API
data Subject = Subject
  { platform :: T.Text
  , images :: Images
  , image :: T.Text
  , summary :: T.Text
  , name :: T.Text
  , name_cn :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Subject
instance ToJSON Subject

data SubjectType = Book | Anime | Music | Game | Life
  deriving (Generic, Show)

instance ToJSON SubjectType where
  toJSON Book = toJSON (1 :: Int)
  toJSON Anime = toJSON (2 :: Int)
  toJSON Music = toJSON (3 :: Int)
  toJSON Game = toJSON (4 :: Int)
  toJSON Life = toJSON (6 :: Int)

-- | Filter options for subject search
newtype SubjectFilter = SubjectFilter
  { subjectType :: Maybe [SubjectType]
  }
  deriving (Generic, Show)

instance ToJSON SubjectFilter where
  toJSON (SubjectFilter types) = object ["type" .= types]

mkSubjectFilter :: [SubjectType] -> SubjectFilter
mkSubjectFilter types = SubjectFilter{subjectType = Just types}

-- | Query parameters for subject search
data SubjectQuery = SubjectQuery
  { keyword :: T.Text
  , filter :: Maybe SubjectFilter
  }
  deriving (Generic, Show)

instance ToJSON SubjectQuery

-- | Helper for search anime subject with keyword
mkAnimeQuery :: T.Text -> SubjectQuery
mkAnimeQuery keyword = SubjectQuery{keyword = keyword, filter = Just $ mkSubjectFilter [Anime]}
