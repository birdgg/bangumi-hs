module MoeWeb.Types (
  MoeHandler,
  RouteEffects,
  MoeEff,
)
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.SQLite (DB)
import Effectful.Time (Time)
import Servant (ServerError)

import Moe.Environment.Env (MoeEnv)
import Moe.Monad (MoeM)
import Moe.ThirdParty (ThirdParty)

type MoeEff = Eff RouteEffects

type MoeHandler a = MoeM RouteEffects a

type RouteEffects =
  '[ DB
   , Time
   , ThirdParty
   , Error ServerError
   , Log
   , Concurrent
   , Reader MoeEnv
   , IOE
   ]
