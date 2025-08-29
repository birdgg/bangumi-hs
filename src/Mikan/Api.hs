module Mikan.Api where

import Data.Proxy
import Data.Text qualified as T
import Mikan.Types
import Servant.API
import Servant.API.ContentTypes.RSS
import Servant.Client

type MikanRssApi = "RSS" :> "Search" :> QueryParam "searchstr" T.Text :> Get '[RSS] MikanRss

mikanRssApi :: Proxy MikanRssApi
mikanRssApi = Proxy

searchMikan :: Maybe T.Text -> ClientM MikanRss
searchMikan = client mikanRssApi
