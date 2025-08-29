module Moe.Environment.Env where

import Data.Pool (Pool)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)
import Moe.Environment.Config
import Network.HTTP.Client (Manager)

data MoeEnv = MoeEnv
  { pool :: Pool SQLite.Connection
  , dbFile :: String
  , httpPort :: Int
  , environment :: DeploymentEnv
  , loggingDestination :: LoggingDestination
  , httpManager :: Manager
  , config :: MoeConfig
  }
  deriving stock (Generic)
