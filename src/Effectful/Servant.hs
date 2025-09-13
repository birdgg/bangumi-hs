{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effectful.Servant where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)

data ServantClient :: Effect where
  Request :: ClientM a -> ServantClient m a

makeEffect ''ServantClient

runServantClient :: (Reader ClientEnv :> es, Error ClientError :> es, IOE :> es) => Eff (ServantClient : es) a -> Eff es a
runServantClient = interpret \_ -> \case
  Request req -> do
    env <- ask @ClientEnv
    res <- liftIO $ runClientM req env
    either throwError pure res
