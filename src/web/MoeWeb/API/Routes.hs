module MoeWeb.API.Routes where

import Moe.Monad (MoeM)
import Moe.Model
import MoeWeb.Types

import Servant

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { bangumi :: mode :- "bangumi" :> Get '[JSON] Bangumi
  } deriving (Generic)

getBangumi :: MoeM RouteEffects Bangumi
getBangumi = pure $ Bangumi "Bangumi"

apiServer :: ServerT Routes MoeEff
apiServer = Routes'
  { bangumi = getBangumi
  }
