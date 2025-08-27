module MoeWeb.Routes where

import Servant.API

import MoeWeb.API.Routes qualified as API

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { api :: mode :- API.Routes
  }
  deriving stock (Generic)
