module BgmTV.Api where

import BgmTV.Types
import RIO (Proxy (..))
import Servant.API
import Servant.Client

type SubjectApi = "search" :> "subjects" :> ReqBody '[JSON] SubjectQuery :> Post '[JSON] (Pagination Subject)

type BgmApi = "v0" :> SubjectApi

bgmApi :: Proxy BgmApi
bgmApi = Proxy

searchSubject :: SubjectQuery -> ClientM (Pagination Subject)
searchSubject = client bgmApi
