module MoeWeb.Types where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Servant (ServerError)

import Moe.Environment.Env

type MoeEff = Eff RouteEffects

type RouteEffects =
  '[ Time
   , Error ServerError
   , Log
   , Concurrent
   , Reader MoeEnv
   , IOE
   ]
