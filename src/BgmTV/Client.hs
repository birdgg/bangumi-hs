module BgmTV.Client (
    BgmClientEnv,
    mkBgmClientEnv,
    HasBgmClientEnv,
    bgmClientEnvL,
    runBgm,
    module BgmTV.Types,
) where

import BgmTV.Types
import Network.HTTP.Client qualified as Client
import RIO
import RIO.Text qualified as T
import Servant.Client
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

newtype BgmClientEnv = BgmClientEnv {unBgmClientEnv :: ClientEnv}

class HasBgmClientEnv env where
    bgmClientEnvL :: Lens' env BgmClientEnv

bgmBaseUrl :: BaseUrl
bgmBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""

bgmUserAgent :: T.Text
bgmUserAgent = "bangumi-hs"

-- | Add User-Agent header to all requests
addUserAgent :: Request -> Request
addUserAgent = addHeader "User-Agent" bgmUserAgent

bgmMiddleware :: ClientMiddleware
bgmMiddleware oapp = oapp . addUserAgent

-- | Create a ClientEnv with User-Agent middleware
mkBgmClientEnv :: Client.Manager -> IO BgmClientEnv
mkBgmClientEnv manager = pure $ BgmClientEnv $ defaultClientEnv{middleware = bgmMiddleware}
  where
    defaultClientEnv = mkClientEnv manager bgmBaseUrl

runBgm :: ClientM a -> BgmClientEnv -> IO (Either ClientError a)
runBgm action = runClientM action . unBgmClientEnv
