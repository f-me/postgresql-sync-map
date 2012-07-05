module Database.PostgreSQL.Syncs (
	sync, syncs,
	field,
	store,
	load,

	TIO, connection,
	create,
	insert, select, update,
	transaction,

	module Database.PostgreSQL.Sync.Base,
	module Database.PostgreSQL.Sync.Types,
	module Database.PostgreSQL.Sync.Condition,
	module Database.PostgreSQL.Sync
	) where

import Control.Arrow
import Database.PostgreSQL.Sync hiding (create, insert, select, update)
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync hiding (create, insert, select, update)
import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Sync.Condition
import qualified Data.Map as M

-- | Make syncs
syncs :: [(String, Sync)] -> [String] -> Syncs
syncs ss cs = result where
	result = Syncs (M.fromList ss) (map (\c -> condition result c []) cs)

-- | Create tables
create :: Syncs -> TIO ()
create = mapM_ S.create . M.elems . syncsSyncs

-- | Perform action on model
withModel :: Syncs -> String -> (Sync -> a) -> a
withModel ss name f = maybe (error "No model with name") f $ M.lookup name (syncsSyncs ss)

-- | Insert Map into postgresql for model
insert :: Syncs -> String -> SyncMap -> TIO ()
insert ss name m = withModel ss name $ \s -> S.insert s m

-- | Select row by condition
select :: Syncs -> String -> Condition -> TIO SyncMap
select ss name c = withModel ss name $ \s -> S.select s c

-- | Update by condition with values, stored in map
update :: Syncs -> String -> Condition -> SyncMap -> TIO ()
update ss name c m = withModel ss name $ \s -> S.update s c m
