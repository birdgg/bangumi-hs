module MoeWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Monad.Except qualified as Except
import Data.Function
import Data.Pool qualified as Pool
import Data.Proxy
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (prettyCallStack, runErrorWith)
import Effectful.Fail (runFailIO)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite (runDB)
import Effectful.Time (runTime)
import Log
import Moe.Environment
import Moe.Environment.Env
import Moe.Logging qualified as Logging
import Moe.Monad (MoeM)
import Moe.ThirdParty (runThirdParty)
import MoeWeb.API.Root qualified as API
import MoeWeb.Common.Tracing
import MoeWeb.Routes
import MoeWeb.Types
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Servant (Application, Context (..), Handler, ServerError (..), serveWithContextT)
import Servant.Server.Generic (AsServerT)

type MoeAuthContext = EmptyContext

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
          let withLogger = Logging.makeLogger env.loggingDestination
          withLogger
            (\appLogger -> runServer appLogger env)
    )

runServer :: (IOE :> es) => Logger -> MoeEnv -> MoeM es ()
runServer appLogger moeEnv = do
  let server = mkServer appLogger moeEnv
  let warpSettings =
        setPort (fromIntegral moeEnv.httpPort) $
          setOnException
            ( handleExceptions
                appLogger
                moeEnv.environment
            )
            defaultSettings
  liftIO $
    runSettings warpSettings server

mkServer :: Logger -> MoeEnv -> Application
mkServer logger moeEnv =
  serveWithContextT
    (Proxy @ServerRoutes)
    EmptyContext
    (naturalTransform moeEnv logger)
    moeServer

shutdownMoe :: MoeEnv -> Eff '[IOE] ()
shutdownMoe env = do
  liftIO $ Pool.destroyAllResources env.pool

moeServer :: Routes (AsServerT MoeEff)
moeServer =
  Routes
    { api = API.apiServer
    }

naturalTransform :: MoeEnv -> Logger -> MoeEff a -> Handler a
naturalTransform moeEnv logger app = do
  result <-
    liftIO $
      Right
        <$> app
        & runDB moeEnv.pool
        & runTime
        & runThirdParty
        & runErrorWith
          ( \callstack err -> do
              Log.logInfo "Server error" $
                object
                  [ "error_headers" .= map show (errHeaders err)
                  , "error_http_code" .= errHTTPCode err
                  , "error_reason_phrase" .= errReasonPhrase err
                  , "exception" .= prettyCallStack callstack
                  ]
              pure . Left $ err
          )
        & Logging.runLog moeEnv.environment logger
        & runConcurrent
        & runReader moeEnv
        & runEff
  either Except.throwError pure result
