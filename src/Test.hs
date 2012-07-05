{-# LANGUAGE OverloadedStrings #-}

module Test (
    test, testMap,
    run
    ) where

import Control.Arrow
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.List (intercalate)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Syncs
import Database.PostgreSQL.Report
import Database.PostgreSQL.Report.Xlsx
import Data.Maybe
import Data.Function
import Data.String

test :: Sync
test = sync "test" "garbage" [
    field "id"   "id"      int,
    field "x"    "xrow"    int,
    field "name" "namerow" string,
    field "i" "irow" string]
    -- reference "i" "irow" "test2"]

test2 :: Sync
test2 = sync "test2" "garbage" [
    field "id"   "id"      int,
    field "car"  "car"     string,
    field "age"  "age"     int]

caseModel :: Sync
caseModel = sync "casetbl" "garbage" [
    field_ "id" int,
--  S.field_ "car_make" S.string,
--  S.field_ "car_program" S.string,
--  S.field_ "car_vin" S.string,
--  S.field_ "car_buyDate" S.time,
--  S.field_ "callDate" S.time,
    field_ "callTaker" string,
    field_ "callerOwner" int,
    field_ "caller_name" string]

tests :: Syncs
tests = syncs [
    ("test", test),
    ("test2", test2),
    ("case", caseModel)] [
    "test.x = test2.id"]

testMap :: SyncMap
testMap = M.fromList [
    ("id", "0"),
    ("x", "123"),
    ("name", "Vasya"),
    ("i", "123"),
    ("blah", "foo"),
    ("quux", "lala")]

testMap2 :: SyncMap
testMap2 = M.fromList [
    ("id", "1"),
    ("x", "222"),
    ("name", "Frodo"),
    ("i", "222"),
    ("blah", "fooo"),
    ("quuz", "alal")]

test2Map :: SyncMap
test2Map = M.fromList [
    ("id", "123"),
    ("car", "ford"),
    ("age", "1")]

test2Map2 :: SyncMap
test2Map2 = M.fromList [
    ("id", "222"),
    ("car", "bentley"),
    ("age", "3")]

local :: ConnectInfo
local = ConnectInfo {
    connectHost = "localhost",
    connectPort = 5432,
    connectUser = "postgres",
    connectPassword = "2741001",
    connectDatabase = "postgres" }

elog :: IO () -> IO ()
elog act = E.catch act onError where
    onError :: E.SomeException -> IO ()
    onError e = putStrLn $ "Failed with: " ++ show e

elogq :: (FromRow q) => IO [q] -> IO [q]
elogq act = E.catch act onError where
    onError :: (FromRow q) => E.SomeException -> IO [q]
    onError e = do
        putStrLn $ "Failed with: " ++ show e
        return []

data AnyValue = AnyValue { toAnyValue ::ByteString }
    deriving (Eq, Ord, Read, Show)

instance FromField AnyValue where
    fromField _ Nothing = return $ AnyValue C8.empty
    fromField _ (Just s) = return $ AnyValue s

run :: IO ()
run = do
    con <- connect local
    elog $ void $ execute_ con "create extension hstore"
    elog $ void $ transaction con $ create tests
    --transaction con $ insert tests "test" testMap
    --transaction con $ insert tests "test" testMap2
    --transaction con $ insert tests "test2" test2Map
    --transaction con $ insert tests "test2" test2Map2
    process [
        ("quit", stop),
        ("drop", takt $ do
            tbl <- getLine
            elog $ void $ execute_ con (fromString $ "drop table " ++ tbl)),
        ("insert", takt $ do
            w <- modelIO
            m <- dataIO
            transaction con $ insert tests w m),
        ("select", takt $ do
            w <- modelIO
            i <- intIO
            m <- transaction con $ select tests w (condition tests "test.id = ?" [toField i])
            print m),
        ("update", takt $ do
            w <- modelIO
            i <- intIO
            m <- dataIO
            transaction con $ update tests w (condition tests "test.id = ?" [toField i]) m),
        ("execute", takt $ do
            q <- queryIO
            anys <- elogq $ query_ con (fromString q)
            mapM_ putStrLn $ map (intercalate " | " . map (C8.unpack . toAnyValue)) anys),
        ("report", takt $ do
            r <- reportIO
            rs <- transaction con $ generate r
            mapM_ putStrLn $ map (intercalate " | " . map show) rs),
        ("reportc", takt $ do
            r <- reportcIO
            rs <- transaction con $ generate r
            mapM_ putStrLn $ map (intercalate " | " . map show) rs),
        ("run-report", takt $ do
            f <- getLine
            t <- getLine
            transaction con $ createReport tests f t)]
    where
        modelIO :: IO String
        modelIO = putStrLn "model:" >> getLine
        intIO :: IO Int
        intIO = putStrLn "index:" >> (getLine >>= readIO)
        dataIO :: IO SyncMap
        dataIO = fmap M.fromList (putStrLn "data:" >> (getLine >>= readIO))
        queryIO :: IO String
        queryIO = putStrLn "query:" >> getLine
        reportIO :: IO Report
        reportIO = do
            putStrLn "fields:"
            fs <- getLine >>= readIO
            putStrLn "conditions:"
            cs <- getLine >>= readIO
            return $ report tests fs (map (\c -> condition tests c []) cs)
        reportcIO :: IO Report
        reportcIO = do
            putStrLn "(field-with-condition)s:"
            fs <- getLine >>= readIO
            return $ reportc tests fs
        
        process ks = do
            k <- getLine
            case lookup k ks of
                Nothing -> do
                    putStrLn $ "Unknown command, possible commands are " ++ (intercalate ", " $ map fst ks)
                    process ks
                Just a -> do
                    b <- a
                    if b then process ks else return ()
        
        stop = return False
        takt act = elog act >> return True
