module Database.PostgreSQL.Report.Xlsx (
	oneRow,
	reportDeclaration,
	generateReport,
	saveReport,
	createReport
	) where

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List
import Data.Ord
import Data.Time.LocalTime
import Data.Time.Clock.POSIX

import Database.PostgreSQL.Report
import Database.PostgreSQL.Sync

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Codec.Xlsx as Xlsx
import qualified Codec.Xlsx.Parser as Xlsx
import qualified Codec.Xlsx.Writer as Xlsx

oneRow :: FilePath -> IO (Maybe (M.Map T.Text T.Text))
oneRow f = do
	x <- Xlsx.xlsx f
	runResourceT $
		Xlsx.sheetRowSource x 0 $$
		CL.peek

reportDeclaration :: FilePath -> IO [(T.Text, T.Text)]
reportDeclaration f = do
	x <- Xlsx.xlsx f
	[k, e] <- runResourceT $
		Xlsx.cellSource x 0 (map Xlsx.int2col [1..10]) $$
		CL.take 2
	let
		toTexts =
			map (toText . Xlsx.cdValue . Xlsx.cellData) .
			sortBy (comparing $ Xlsx.col2int . fst . Xlsx.cellIx)
		toText Nothing = T.empty
		toText (Just (Xlsx.CellText t)) = t
		toText (Just _) = T.empty
	print $ toTexts k
	print $ toTexts e
	return $ zip (toTexts k) (toTexts e)

generateReport :: Syncs -> [(T.Text, T.Text)] -> TIO [[FieldValue]]
generateReport ss m = generate (reportc ss m') where
	m' = map T.unpack $ map snd m

saveReport :: FilePath -> [T.Text] -> [[FieldValue]] -> IO ()
saveReport f ts fs = Xlsx.writeXlsx f [sheet] where
	allRows = names : fs
	names = map (StringValue . T.unpack) ts
	sheet = Xlsx.Worksheet {
		Xlsx.wsName = T.pack "report",
		Xlsx.wsMinX = 1,
		Xlsx.wsMaxX = 1 + length ts,
		Xlsx.wsMinY = 1,
		Xlsx.wsMaxY = 1 + length allRows,
		Xlsx.wsColumns = [],
		Xlsx.wsRowHeights = M.empty,
		Xlsx.wsCells = cells }
	cells = M.unions $ zipWith row [1..] allRows
	row r rowData = M.unions $ zipWith (cell r) [1..] rowData
	cell r c d = M.singleton (c, r) (fieldValueToCell d)

createReport :: Syncs -> FilePath -> FilePath -> TIO ()
createReport ss from to = do
	reportDecl <- liftIO $ reportDeclaration from
	fs <- generateReport ss reportDecl
	liftIO $ saveReport to (map fst reportDecl) fs

fieldValueToCell :: FieldValue -> Xlsx.CellData
fieldValueToCell (IntValue i) = cell $ Xlsx.CellText $ T.pack $ show i
fieldValueToCell (DoubleValue i) = cell $ Xlsx.CellDouble i
fieldValueToCell (BoolValue i) = cell $ Xlsx.CellText $ T.pack $ show i
fieldValueToCell (StringValue i) = cell $ Xlsx.CellText $ T.pack i
fieldValueToCell (TimeValue i) = cell $ Xlsx.CellLocalTime $ toLocalTime i where
	toLocalTime = utcToLocalTime utc . posixSecondsToUTCTime
fieldValueToCell (HStoreValue i)
	| M.null i = cell $ Xlsx.CellText $ T.empty
	| otherwise = error $ "HStore can't be in one cell: " ++ show i

cell :: Xlsx.CellValue -> Xlsx.CellData
cell = Xlsx.CellData Nothing . Just