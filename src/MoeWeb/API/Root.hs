{-# LANGUAGE AllowAmbiguousTypes #-}

module MoeWeb.API.Root where

import MoeWeb.API.Search
import MoeWeb.Types
import Servant
import Servant.API.Generic

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { search :: mode :- SearchRoute
  }
  deriving (Generic)

apiServer :: ServerT Routes MoeEff
apiServer =
  Routes'
    { search = searchServer
    }
