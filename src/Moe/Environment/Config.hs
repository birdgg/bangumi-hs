module Moe.Environment.Config where

import Data.Text.Display (Display (..))
import Env (
  AsUnread (unread),
  Error (..),
  Parser,
  Reader,
  def,
  var,
 )
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data DeploymentEnv
  = Production
  | Development
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance Display DeploymentEnv where
  displayBuilder Production = "production"
  displayBuilder Development = "development"

data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Generic, Show)

-- | The datatype that is used to model the external configuration
data MoeConfig = MoeConfig
  { httpPort :: Int
  , environment :: DeploymentEnv
  }
  deriving stock (Generic, Show)

parsePort :: Parser Error Int
parsePort = var port "HTTP_PORT" (def 3000)

parseDeploymentEnv :: Parser Error DeploymentEnv
parseDeploymentEnv =
  var deploymentEnv "ENVIRONMENT" (def Development)

parseConfig :: Parser Error MoeConfig
parseConfig =
  MoeConfig
    <$> parsePort
    <*> parseDeploymentEnv

-- Env parser helpers

int :: Env.Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just i' -> Right i'

port :: Env.Reader Error Int
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right intPort
      else Left . unread . show $ p

deploymentEnv :: Env.Reader Error DeploymentEnv
deploymentEnv "production" = Right Production
deploymentEnv "development" = Right Development
deploymentEnv e = Left $ unread e
