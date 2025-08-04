module Moe.Server.Api where

import BgmTV.Client
import Moe.Config
import RIO
import RIO.Text qualified as T
import Servant.API
import Servant.Server

type SearchApi = "search" :> "bangumi" :> QueryParam "keyword" T.Text :> Get '[JSON] [Subject]

-- | Handlers
searchBangumiHandler :: Maybe T.Text -> AppM Config [Subject]
searchBangumiHandler keyword = do
    env <- ask
    case keyword of
        Nothing -> throwIO $ err400{errBody = "Your credentials are invalid."}
        Just k -> liftIO $ runBgm (searchSubject (mkAnimeQuery k)) env.bgmClientEnv

mkAnimeQuery :: T.Text -> SubjectQuery
mkAnimeQuery k = mkSubjectQuery k Anime
