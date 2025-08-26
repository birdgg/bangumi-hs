{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moe.Model where

import GHC.Generics

import Data.Aeson
import Database.Persist ()
import Database.Persist.Sqlite ()
import Database.Persist.TH



share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Bangumi
    name String
    deriving Show Generic
|]

instance ToJSON Bangumi
