module MoeWeb.Types where

import Effectful

type MoeEff = Eff RouteEffects

type RouteEffects =
  '[IOE
   ]
