{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module BgmTV.Types.Subject where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Pagination wrapper for API responses
data Pagination a = Pagination
    { pData :: [a]
    }
    deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Pagination a) where
    parseJSON = withObject "Pagination" $ \o -> do
        dat <- o .: "data"
        return $ Pagination dat

-- | Image URLs in different sizes
data Images = Images
    { small :: Text
    , grid :: Text
    , large :: Text
    , medium :: Text
    , common :: Text
    }
    deriving (Generic, Show)

instance FromJSON Images

-- | Subject information from BGM.TV API
data Subject = Subject
    { platform :: Text
    , images :: Images
    , image :: Text
    , summary :: Text
    , name :: Text
    , name_cn :: Text
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
data SubjectFilter = SubjectFilter
    { subjectType :: Maybe [SubjectType]
    }
    deriving (Generic, Show)

instance ToJSON SubjectFilter where
    toJSON (SubjectFilter types) = object ["type" .= types]

mkSubjectFilter :: [SubjectType] -> SubjectFilter
mkSubjectFilter types = SubjectFilter{subjectType = Just types}

-- | Query parameters for subject search
data SubjectQuery = SubjectQuery
    { keyword :: Text
    , filter :: Maybe SubjectFilter
    }
    deriving (Generic, Show)

instance ToJSON SubjectQuery

mkSubjectQuery :: Text -> SubjectType -> SubjectQuery
mkSubjectQuery keyword t = SubjectQuery{keyword = keyword, filter = Just $ mkSubjectFilter [t]}
