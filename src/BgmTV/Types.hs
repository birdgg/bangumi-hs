module BgmTV.Types where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import RIO
import RIO.Text qualified as T

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

-- | Subject information from BGM.TV API
data Subject = Subject
    { platform :: T.Text
    , images :: Images
    , image :: T.Text
    , summary :: Text
    , name :: T.Text
    , name_cn :: T.Text
    }
    deriving (Generic, Show)

instance FromJSON Subject

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

mkSubjectQuery :: T.Text -> SubjectType -> SubjectQuery
mkSubjectQuery keyword t = SubjectQuery{keyword = keyword, filter = Just $ mkSubjectFilter [t]}
