module Server.Api.Bangumi where

import BgmTV.Types
import RIO.Text qualified as T
import Servant.API

type BangumiApi = "bangumi" :> "search" :> QueryParam "keyword" T.Text :> Get '[JSON] [Subject]

-- serverBangumi :: Maybe Text -> Handler [Subject]
-- serverBangumi query = do
