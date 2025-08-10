module BgmTV.Client (
    mkBgmClientEnv,
    HasBgmClientEnv,
    bgmClientEnvL,
    searchSubject,
    module BgmTV.Types,
) where

import BgmTV.Api
import BgmTV.Types
import Network.HTTP.Client qualified as Client
import RIO
import RIO.Text qualified as T
import Servant.Client
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

-- newtype BgmClientEnv = BgmClientEnv {unBgmClientEnv :: ClientEnv}

class HasBgmClientEnv env where
    bgmClientEnvL :: Lens' env ClientEnv

bgmBaseUrl :: BaseUrl
bgmBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""

bgmUserAgent :: T.Text
bgmUserAgent = "bangumi-hs"

bgmApi :: Proxy BgmApi
bgmApi = Proxy

searchSubject :: SubjectQuery -> ClientM (Pagination Subject)
searchSubject = client bgmApi

addUserAgent :: Request -> Request
addUserAgent = addHeader "User-Agent" bgmUserAgent

bgmMiddleware :: ClientMiddleware
bgmMiddleware oapp = oapp . addUserAgent

mkBgmClientEnv :: Client.Manager -> IO ClientEnv
mkBgmClientEnv manager = pure defaultClientEnv{middleware = bgmMiddleware}
  where
    defaultClientEnv = mkClientEnv manager bgmBaseUrl
