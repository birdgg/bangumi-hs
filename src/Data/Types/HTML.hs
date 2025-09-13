module Data.Types.HTML where

import Data.Text qualified as T
import Text.HTML.Scalpel.Core (Scraper)

class FromHTML a where
  fromHTML :: Scraper T.Text a
