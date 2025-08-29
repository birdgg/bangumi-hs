{-# LANGUAGE TemplateHaskell #-}

module Effectful.SQLite where

import qualified Database.SQLite.Simple as SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import UnliftIO.Pool as Pool

data DB :: Effect where
  DB :: (SQLite.Connection -> m a) -> DB m a

makeEffect ''DB

runDB ::
  (HasCallStack, IOE :> es) =>
  Pool.Pool SQLite.Connection ->
  Eff (DB : es) a ->
  Eff es a
runDB pool = interpret $ \env -> \case
  DB f ->
    localSeqUnlift env $ \unlift -> do
      Pool.withResource pool $ unlift . f
