module BgmTV.Client where

import Network.HTTP.Client qualified as Client
import RIO
import RIO.Text qualified as T
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (..), middleware, mkClientEnv)
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

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
mkBgmClientEnv :: Client.Manager -> ClientEnv
mkBgmClientEnv manager = defaultClientEnv{middleware = bgmMiddleware}
  where
    defaultClientEnv = mkClientEnv manager bgmBaseUrl
