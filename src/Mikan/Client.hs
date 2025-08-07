module Mikan.Client (
    mkMikanClientEnv,
    HasMikanClientEnv (..),
    module Mikan.Api,
    module Mikan.Types,
)
where

import Mikan.Api
import Mikan.Types
import Network.HTTP.Client qualified as Client
import RIO
import Servant.Client

mikanBaseUrl :: BaseUrl
mikanBaseUrl = BaseUrl Https "mikanani.me" 443 ""

class HasMikanClientEnv env where
    mikanClientEnvL :: Lens' env ClientEnv

mkMikanClientEnv :: Client.Manager -> IO ClientEnv
mkMikanClientEnv manager = pure $ mkClientEnv manager mikanBaseUrl
