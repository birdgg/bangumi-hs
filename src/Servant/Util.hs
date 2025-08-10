module Servant.Util where

import Data.Aeson
import RIO
import RIO.Text qualified as T
import Servant.API
import Servant.Client
import Servant.Server

-- | Required Query Parameter
type RQueryParam = QueryParam' '[Required, Strict]

-- | Throw ClientError as server 400 error
runClientM_ :: (MonadIO m, MonadThrow m) => ClientM a -> ClientEnv -> m a
runClientM_ clientM clientEnv =
    liftIO (runClientM clientM clientEnv)
        >>= either
            (throwM . errClient)
            pure

-- Error Utils
newtype ErrorResponse = ErrorResponse
    { error :: T.Text
    }
    deriving (Generic, Show)

instance ToJSON ErrorResponse

jsonFormatter :: ErrorFormatter
jsonFormatter _ _ = jsonErr err400 . fromString

jsonErrorFormatters :: ErrorFormatters
jsonErrorFormatters =
    defaultErrorFormatters
        { bodyParserErrorFormatter = jsonFormatter
        , urlParseErrorFormatter = jsonFormatter
        }

errClient :: ClientError -> ServerError
errClient = jsonErr err400 . fromString . show

jsonErr :: ServerError -> T.Text -> ServerError
jsonErr err msg =
    err
        { errBody = encode $ ErrorResponse msg
        , errHeaders = [("Content-Type", "application/json")]
        }
