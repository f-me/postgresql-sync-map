{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

-- | To make convertor to and from specified map, use list of field connectors:
-- @
-- test = sync "table" "idcolumn" "garbage" [
--   field "x"     "xrow"     int,     -- ^ Store 'x' as int in 'xrow'
--   field "y"     "yrow"     int,     -- ^ Store 'y' as int in 'yrow'
--   field "name"  "namerow"  string]  -- ^ Store 'name' as string in 'namerow'
-- @
-- Now use it for reading-storing rows
-- @
-- -- Insert data
-- insert con test (Just 10) (M.fromList [("x", "123"), ("y", "22"), ("name", "Vasya"), ("blah", "blahblah"), ("foo", "bar")])
-- -- Select data by id
-- m <- select con test 10
-- -- Update specified fields (and also adds 'qoo' to hstore) by id
-- update con test 10 (M.fromList [("x", "333"), ("foo", "baz"), ("qoo", "aaa")]
-- @
module Database.PostgreSQL.Sync (
    TIO, connection,
    transaction, inPG,
    
    module Database.PostgreSQL.Sync.Base,
    module Database.PostgreSQL.Sync.Types,
    module Database.PostgreSQL.Sync.Condition
    ) where

import Prelude hiding (log, catch)

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.CatchIO
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Sync.Base
import Database.PostgreSQL.Sync.Types
import Database.PostgreSQL.Sync.Condition
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import qualified Database.PostgreSQL.Simple.ToRow as PG
import Data.List hiding (insert)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import System.Log.Simple

data SyncConnection = SyncConnection {
    syncConn :: Connection,
    syncLog :: Log }

newtype TIO a = TIO (ReaderT SyncConnection IO a)
    deriving (Monad, Functor, Applicative, MonadIO, MonadCatchIO)

-- | Get connection inside monad
connection :: TIO Connection
connection = TIO $ asks syncConn

instance MonadLog TIO where
    askLog = TIO $ asks syncLog

           
-- | Transaction
transaction :: Connection -> TIO a -> ReaderT Log IO a
transaction con (TIO act) = do
    l <- askLog
    liftIO $ runReaderT act (SyncConnection con l)
--transaction con (TIO act) = withTransaction con (runReaderT act con)

-- | For now there is no module for use with snaplet-postgresql-simple,
-- but you can use functions using this simple wrap:
-- withPG (inPG $ update ...)
inPG :: TIO a -> Connection -> ReaderT Log IO a
inPG = flip transaction
