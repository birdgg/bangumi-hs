module Moe.Monad where

import Effectful

type MoeM es a = Eff es a
