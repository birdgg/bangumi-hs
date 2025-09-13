module MoeWeb.API.Search where

import BgmTV.Client qualified as BgmTV
import Data.Text qualified as T
import Mikan.Client qualified as Mikan
import Moe.ThirdParty
import MoeWeb.Types (MoeEff, MoeHandler)

import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Util

type SearchRoute = "search" :> NamedRoutes SearchRoute'

-- | search with keyword
type Keyword = RQueryParam "keyword" T.Text

data SearchRoute' mode = SearchRoute'
  { searchBangumi :: mode :- "bangumi" :> Keyword :> Get '[JSON] [BgmTV.Subject]
  , searchRss :: mode :- "rss" :> Keyword :> Get '[JSON] [T.Text]
  }
  deriving (Generic)

searchServer :: ServerT SearchRoute MoeEff
searchServer =
  SearchRoute'
    { searchBangumi = searchBangumiHandler
    , searchRss = searchRssHandler
    }

-- Handlers
searchBangumiHandler :: T.Text -> MoeHandler [BgmTV.Subject]
searchBangumiHandler keyword = do
  let query = BgmTV.mkAnimeQuery keyword
  r <- request BgmTV (BgmTV.searchSubject query)
  pure r.pData

searchRssHandler :: T.Text -> MoeHandler [T.Text]
searchRssHandler keyword = do
  _ <- request Mikan (Mikan.searchMikan $ Just keyword)
  pure []
