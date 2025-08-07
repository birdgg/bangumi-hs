module Moe.Api.Search where

import BgmTV.Client
import Mikan.Client
import Moe.Config
import RIO
import RIO.Text qualified as T
import Rss.Types
import Servant.API
import Servant.Client
import Servant.Server

type SearchApi =
    "search"
        :> ( "bangumi" :> QueryParam "keyword" T.Text :> Get '[JSON] [Subject]
                :<|> "mikan" :> QueryParam "keyword" T.Text :> Get '[JSON] [MikanRssItem]
           )

-- | Handlers
searchBangumiHandler :: (HasBgmClientEnv env) => Maybe T.Text -> AppM env [Subject]
searchBangumiHandler Nothing = throwIO $ err400{errBody = "Keyword needed"}
searchBangumiHandler (Just k) = do
    bgmClientEnv <- view bgmClientEnvL
    result <- liftIO $ runBgm (searchSubject (mkAnimeQuery k)) bgmClientEnv
    pure $ either (const []) (.pData) result

searchMikanHandler :: (HasMikanClientEnv env) => Maybe T.Text -> AppM env [MikanRssItem]
searchMikanHandler k = do
    mikanClientEnv <- view mikanClientEnvL
    result <- liftIO $ runClientM (searchMikan k) mikanClientEnv
    case result of
        Left _ -> throwIO $ err400{errBody = "some err"}
        Right rss -> pure rss.items

mkAnimeQuery :: T.Text -> SubjectQuery
mkAnimeQuery k = mkSubjectQuery k Anime

serverSearch :: (HasBgmClientEnv env, HasMikanClientEnv env) => ServerT SearchApi (AppM env)
serverSearch = searchBangumiHandler :<|> searchMikanHandler
