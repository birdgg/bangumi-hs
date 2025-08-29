module Moe.Environment where

import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Pool.Introspection (defaultPoolConfig)
import Data.Time (NominalDiffTime)
import Database.SQLite.Simple qualified as SQLite
import Effectful
import Effectful.Fail (Fail)
import Env (parse)
import Moe.Environment.Config
import Moe.Environment.Env
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

mkPool ::
  (IOE :> es) =>
  String -> -- Database access information
  NominalDiffTime -> -- Allowed timeout
  Int -> -- Number of connections
  Eff es (Pool SQLite.Connection)
mkPool connectionInfo timeout' connections =
  liftIO $
    Pool.newPool $
      defaultPoolConfig
        (SQLite.open connectionInfo)
        SQLite.close
        (realToFrac timeout')
        connections

getDbFile :: DeploymentEnv -> String
getDbFile Production = "production.db"
getDbFile Development = "development.db"

configToEnv :: (Fail :> es, IOE :> es) => MoeConfig -> Eff es MoeEnv
configToEnv moeConfig = do
  let dbFile = getDbFile moeConfig.environment
  pool <- mkPool dbFile 60 10
  httpManager <- liftIO $ newManager tlsManagerSettings
  pure $
    MoeEnv
      { pool = pool
      , httpPort = moeConfig.httpPort
      , environment = moeConfig.environment
      , dbFile = dbFile
      , loggingDestination = Json
      , httpManager = httpManager
      , config = moeConfig
      }

getMoeEnv :: (Fail :> es, IOE :> es) => Eff es MoeEnv
getMoeEnv = do
  config <- liftIO $ Env.parse id parseConfig
  configToEnv config
