{-# LANGUAGE TemplateHaskell #-}

module Moe.ThirdParty where

import BgmTV.Client
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text.Display (display)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import Mikan.Client
import Moe.Environment.Env
import Moe.Monad
import Network.HTTP.Client qualified as Client
import Servant.Client
import Servant.Display ()
import Servant.Server (ServerError, err400, errBody)

data RequestTarget = BgmTV | Mikan

-- | ThirdPartyApi Effect
data ThirdParty :: Effect where
  Request :: RequestTarget -> ClientM a -> ThirdParty m a

makeEffect ''ThirdParty

runThirdParty :: (Reader MoeEnv :> es, Error ServerError :> es, IOE :> es) => Eff (ThirdParty : es) a -> Eff es a
runThirdParty = interpret \_ -> \case
  Request target req -> do
    MoeEnv{httpManager} <- ask
    let clientEnv = mkClient target httpManager
    res <- liftIO $ runClientM req clientEnv
    either clientFailed pure res

mkClient :: RequestTarget -> Client.Manager -> ClientEnv
mkClient BgmTV = mkBgmClientEnv
mkClient Mikan = mkMikanClientEnv

-- Helpers
clientFailed :: (Error ServerError :> es) => ClientError -> MoeM es a
clientFailed err =
  throwError
    err400
      { errBody = jsonErrBody $ display err
      }

jsonErrBody :: (ToJSON a) => a -> ByteString
jsonErrBody detail = encode $ object ["detail" .= detail]
