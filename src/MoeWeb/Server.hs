module MoeWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Monad.Except qualified as Except
import Data.Function
import Data.Proxy
import Data.Text.Display (display)

import Effectful
import Effectful.Concurrent
import Effectful.Fail (runFailIO)

import Moe.Environment
import Moe.Environment.Env
import Moe.Monad (MoeM)

import Data.Pool qualified as Pool
import MoeWeb.API.Routes qualified as API
import MoeWeb.Routes
import MoeWeb.Types
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, serveWithContextT)
import Servant.Server (Context (..))
import Servant.Server.Generic (AsServerT)

-- TODO: run database migration
runMoe :: IO ()
runMoe = do
  bracket
    (getMoeEnv & runFailIO & runEff)
    (runEff . shutdownMoe)
    ( \env ->
        runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runConcurrent $ do
          let baseURL = "http://localhost:" <> display env.httpPort
          liftIO $ blueMessage $ "🌺 Starting Moe server on " <> baseURL
          runServer
          -- let withLogger = Logging.makeLogger env.mltp.logger
          -- withLogger
          --   ( \appLogger ->
          --       provideCallStack $ runServer appLogger env
          --   )
    )

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

shutdownMoe :: MoeEnv -> Eff '[IOE] ()
shutdownMoe env = do
  liftIO $ Pool.destroyAllResources env.pool

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
