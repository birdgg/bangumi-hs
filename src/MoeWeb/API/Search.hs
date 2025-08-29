module MoeWeb.API.Search where

import BgmTV.Client
import Data.Text
import Moe.Monad
import Moe.ThirdParty
import MoeWeb.API.Errors
import MoeWeb.Types
import Servant.API
import Servant.API.Generic
import Servant.Server

type SearchRoute = "search" :> NamedRoutes SearchRoute'

data SearchRoute' mode = SearchRoute'
  { searchBangumi :: mode :- "bangumi" :> QueryParam "keyword" Text :> Get '[JSON] [Subject]
  }
  deriving stock (Generic)

searchServer :: ServerT SearchRoute MoeEff
searchServer =
  SearchRoute'
    { searchBangumi = searchBangumiHandler
    }

-- Handlers
searchBangumiHandler :: Maybe Text -> MoeM RouteEffects [Subject]
searchBangumiHandler keyword = do
  case keyword of
    Nothing -> pure []
    Just k -> do
      let query = mkAnimeQuery k
      result <- requestThirdParty BgmTV (searchSubject query)
      case result of
        Left err -> clientFailed err
        Right response -> pure response.pData
