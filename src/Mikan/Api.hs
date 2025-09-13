module Mikan.Api where

import Data.Proxy
import Data.Text qualified as T
import Mikan.Types
import Servant.API
import Servant.API.ContentTypes.HTML (HTML)
import Servant.API.ContentTypes.RSS
import Servant.Client

type MikanApi =
  "RSS" :> "Search" :> QueryParam "searchstr" T.Text :> Get '[RSS] MikanRss
    :<|> "Home" :> "Search" :> QueryParam "searchstr" T.Text :> Get '[HTML] BangumiList

-- :<|> "Home" :> "Bangumi" :> Capture "id" Int :> Get '[HTML] ()

mikanApi :: Proxy MikanApi
mikanApi = Proxy

searchMikan :: Maybe T.Text -> ClientM MikanRss
searchBangumi :: Maybe T.Text -> ClientM BangumiList
(searchMikan :<|> searchBangumi) = client mikanApi
