module MoeWeb.API.Errors where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text.Display (display)
import Effectful
import Effectful.Error.Static
import Moe.Monad
import Servant.Client
import Servant.Display ()
import Servant.Server

-- | Helper for handling client errors.
clientFailed :: (Error ServerError :> es) => ClientError -> MoeM es a
clientFailed err =
  throwError
    err400
      { errBody = jsonErrBody $ display err
      }

jsonErrBody :: (ToJSON a) => a -> ByteString
jsonErrBody detail = encode $ object ["detail" .= detail]
