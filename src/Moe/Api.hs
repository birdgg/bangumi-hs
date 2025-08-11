module Moe.Api where

import Servant.API
import Moe.Api.Search

type Api = "api" :> (SearchApi)
