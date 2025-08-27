module BgmTV.Client (
  mkBgmClientEnv,
  searchSubject,
  module BgmTV.Types,
) where

import BgmTV.Api
import BgmTV.Types
import Data.Text qualified as T
import Network.HTTP.Client qualified as Client
import Servant.Client
import Servant.Client.Core (Request, addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

-- newtype BgmClientEnv = BgmClientEnv {unBgmClientEnv :: ClientEnv}

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
