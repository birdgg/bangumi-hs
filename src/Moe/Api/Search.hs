module Moe.Api.Search where

import BgmTV.Client
import Mikan.Client
import Moe.Config
import RIO
import RIO.Text qualified as T
import Rss.Types
import Servant.API
import Servant.Server
import Servant.Util

type SearchApi =
    "search"
        :> ( "bangumi" :> RQueryParam "keyword" T.Text :> Get '[JSON] [Subject]
                :<|> "rss" :> RQueryParam "keyword" T.Text :> Get '[JSON] [MikanRssItem]
           )

searchBangumiHandler :: (HasBgmClientEnv env) => T.Text -> AppM env [Subject]
searchBangumiHandler k = do
    bgmClientEnv <- view bgmClientEnvL
    result <- runClientM_ (searchSubject (mkAnimeQuery k)) bgmClientEnv
    pure result.pData

searchMikanHandler :: (HasMikanClientEnv env) => T.Text -> AppM env [MikanRssItem]
searchMikanHandler k = do
    mikanClientEnv <- view mikanClientEnvL
    result <- liftIO $ runClientM_ (searchMikan $ Just k) mikanClientEnv
    pure result.items

mkAnimeQuery :: T.Text -> SubjectQuery
mkAnimeQuery k = mkSubjectQuery k Anime

serverSearch :: (HasBgmClientEnv env, HasMikanClientEnv env) => ServerT SearchApi (AppM env)
serverSearch = searchBangumiHandler :<|> searchMikanHandler
