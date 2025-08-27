module MoeWeb.Server where

import Control.Monad.Except qualified as Except
import Effectful
import Moe.Monad (MoeM)
import MoeWeb.API.Routes qualified as API
import MoeWeb.Routes
import MoeWeb.Types
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, serveWithContextT)
import Servant.Server (Context (..))
import Servant.Server.Generic (AsServerT)

runMoe :: IO ()
runMoe = do
  runEff runServer

runServer :: (IOE :> es) => MoeM es ()
runServer = do
  let server = mkServer
  liftIO $ run 3000 server

mkServer :: Application
mkServer =
  serveWithContextT
    (Proxy @ServerRoutes)
    EmptyContext
    naturalTransform
    moeServer

moeServer :: Routes (AsServerT MoeEff)
moeServer =
  Routes
    { api = API.apiServer
    }

naturalTransform :: MoeEff a -> Handler a
naturalTransform app = do
  result <-
    liftIO $
      Right
        <$> app
        & runEff
  either Except.throwError pure result
