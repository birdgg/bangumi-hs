module MoeWeb.Routes where

import Servant.API

import GHC.Generics (Generic)
import MoeWeb.API.Root qualified as API

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { api :: mode :- API.Routes
  }
  deriving stock (Generic)
