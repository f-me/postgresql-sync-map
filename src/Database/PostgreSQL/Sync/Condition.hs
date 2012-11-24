module Database.PostgreSQL.Sync.Condition (
    toWhere, affects,
    conditionSimple,
    FieldName,
    splitField, catField,
    parseRelation,

    module Database.PostgreSQL.Sync.Base
    ) where

import Control.Arrow
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Data.Char
import Data.Either
import qualified Data.Text as T
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))

instance Monoid Condition where
    mempty = Condition [] [] "" []
    mappend (Condition [] [] "" []) r = r
    mappend l (Condition [] [] "" []) = l
    mappend (Condition tl fl sl al) (Condition tr fr sr ar) = Condition
        (nub $ tl ++ tr)
        (nub $ fl ++ fr)
        (sl ++ " and " ++ sr)
        (al ++ ar)

toWhere :: Condition -> String
toWhere c
	| conditionString c == "" = ""
	| otherwise = " where " ++ conditionString c

-- | Check whether condition affects tables specified
affects :: [String] -> Condition -> Bool
affects tables cond = all (`elem` tables) $ conditionTablesAffected cond

-- | Create condition on one field and table
conditionSimple :: String -> String -> (String -> String) -> [Action] -> Condition
conditionSimple table field fcond acts = Condition [table] [field] (fcond (table ++ "." ++ field)) acts

type FieldName = (String, String)

-- | Split model.name to (model, name)
splitField :: String -> Maybe FieldName
splitField str = if valid then Just (model, name) else Nothing where
	valid = all (\c -> isAlpha c || isDigit c || c `elem` "._") str
	(model, name) = second (drop 1) $ break (== '.') str

-- | Concat (table, field) to table.field
catField :: FieldName -> String
catField (model, name) = model ++ "." ++ name

parseRelation :: String -> Condition
parseRelation v = Condition tables fields v [] where
    (tables, fields) = unzip $ mapMaybe parseField $ words v
    parseField f = case break (== '.') f of
        (_, "") -> Nothing
        (tbl, col) -> Just (tbl, tail col)
