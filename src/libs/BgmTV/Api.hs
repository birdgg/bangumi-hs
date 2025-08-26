module BgmTV.Api where

import BgmTV.Types
import Servant.API

type SubjectApi = "search" :> "subjects" :> ReqBody '[JSON] SubjectQuery :> Post '[JSON] (Pagination Subject)

type BgmApi = "v0" :> SubjectApi
