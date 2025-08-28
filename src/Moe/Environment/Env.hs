module Moe.Environment.Env where

import Data.Pool (Pool)
import Database.SQLite.Simple qualified as SQLite

import Moe.Environment.Config

data MoeEnv = MoeEnv
  { pool :: Pool SQLite.Connection
  , dbFile :: String
  , httpPort :: Int
  , environment :: DeploymentEnv
  , config :: MoeConfig
  }
  deriving stock (Generic)
