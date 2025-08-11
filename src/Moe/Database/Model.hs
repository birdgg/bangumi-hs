{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Moe.Database.Model where

import Database.Persist.TH
import RIO
import RIO.Text qualified as T
import RIO.Time

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Bangumi
    titleZh T.Text
    titleJp T.Text
    season Int
    cover T.Text
    group T.Text
    totalEps Int
    currentEp Int
    tags [T.Text]
    rss T.Text
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Eq
|]