module Server.Api.Bangumi where

import Data.Text
import Servant.API

type BangumiApi = "bangumi" :> "search" :> QueryParam "keyword" Text
