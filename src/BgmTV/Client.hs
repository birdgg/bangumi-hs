module BgmTV.Client (
  mkBgmClientEnv,
  searchSubject,
  module BgmTV.Types,
) where

import BgmTV.Types
import Data.Proxy
import Data.Text
import Network.HTTP.Client qualified as Client
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core (addHeader)
import Servant.Client.Internal.HttpClient (ClientMiddleware)

type SubjectApi = "search" :> "subjects" :> ReqBody '[JSON] SubjectQuery :> Post '[JSON] (Pagination Subject)

type BgmApi = "v0" :> SubjectApi

bgmApi :: Proxy BgmApi
bgmApi = Proxy

searchSubject :: SubjectQuery -> ClientM (Pagination Subject)
searchSubject = client bgmApi

bgmMiddleware :: ClientMiddleware
bgmMiddleware oapp = oapp . addHeader "User-Agent" userAgent
 where
  userAgent = "bangumi-hs" :: Text

mkBgmClientEnv :: Client.Manager -> ClientEnv
mkBgmClientEnv manager = defaultClientEnv{middleware = bgmMiddleware}
 where
  defaultClientEnv = mkClientEnv manager bgmBaseUrl
  bgmBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""
