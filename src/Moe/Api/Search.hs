module Moe.Api.Search where

import BgmTV.Client
import Moe.Config
import RIO
import RIO.Text qualified as T
import Servant.API
import Servant.Server

type BangumiApi =
    "bangumi"
        :> ( "search" :> QueryParam "keyword" T.Text :> Get '[JSON] [Subject]
        -- :<|> "rss" :> QueryParam "keyword" T.Text :> Get '[JSON] [Subject]
           )

-- | Handlers
searchBangumiHandler :: (HasBgmClientEnv env) => Maybe T.Text -> AppM env [Subject]
searchBangumiHandler Nothing = throwIO $ err400{errBody = "Keyword needed"}
searchBangumiHandler (Just k) = do
    bgmClientEnv <- view bgmClientEnvL
    result <- liftIO $ runBgm (searchSubject (mkAnimeQuery k)) bgmClientEnv
    pure $ either (const []) (.pData) result

mkAnimeQuery :: T.Text -> SubjectQuery
mkAnimeQuery k = mkSubjectQuery k Anime

serverBangumi :: ServerT BangumiApi (AppM Config)
serverBangumi = searchBangumiHandler
