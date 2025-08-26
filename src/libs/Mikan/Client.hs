module Mikan.Client (
  mkMikanClientEnv,
  module Mikan.Api,
  module Mikan.Types,
)
where

import Mikan.Api
import Mikan.Types
import Network.HTTP.Client qualified as Client
import Servant.Client

mikanBaseUrl :: BaseUrl
mikanBaseUrl = BaseUrl Https "mikanani.me" 443 ""

mkMikanClientEnv :: Client.Manager -> IO ClientEnv
mkMikanClientEnv manager = pure $ mkClientEnv manager mikanBaseUrl
