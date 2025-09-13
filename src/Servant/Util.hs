{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Util where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics
import Servant.API
import Servant.Server

-- | Required Query Parameter
type RQueryParam = QueryParam' '[Required, Strict]

-- Error Utils
newtype ErrorResponse = ErrorResponse
  { error :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON ErrorResponse

-- jsonFormatter :: ErrorFormatter
-- jsonFormatter _ _ = jsonErr err400 . fromString

-- jsonErrorFormatters :: ErrorFormatters
-- jsonErrorFormatters =
--     defaultErrorFormatters
--         { bodyParserErrorFormatter = jsonFormatter
--         , urlParseErrorFormatter = jsonFormatter
--         }

-- errClient :: ClientError -> ServerError
-- errClient = jsonErr err400 . fromString . show

jsonErr :: ServerError -> T.Text -> ServerError
jsonErr err msg =
  err
    { errBody = encode $ ErrorResponse msg
    , errHeaders = [("Content-Type", "application/json")]
    }
