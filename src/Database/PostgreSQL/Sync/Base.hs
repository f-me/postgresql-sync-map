module Database.PostgreSQL.Sync.Base (
    Condition(..), Relations(..)
    ) where

import qualified Data.Map as M
import Database.PostgreSQL.Sync.Types

-- | Condition on query, containts tables affected, condition string with placeholders ('?') and arguments
data Condition = Condition {
    conditionTablesAffected :: [String],
    conditionFieldsAffected :: [String],
    conditionString :: String,
    conditionArguments :: [Action] }
        deriving (Show)

data Relations = Relations {
    relationsConditions :: [Condition] }
        deriving (Show)
