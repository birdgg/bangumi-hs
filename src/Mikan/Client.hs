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

mkMikanClientEnv :: Client.Manager -> ClientEnv
mkMikanClientEnv manager = mkClientEnv manager mikanBaseUrl
