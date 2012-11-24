{-# LANGUAGE OverloadedStrings #-}

-- | Provides loading Sync and Syncs from JSON
module Database.PostgreSQL.Sync.JSON (
    ) where

import Control.Applicative
import Data.Aeson
import qualified Data.Map as M

import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
