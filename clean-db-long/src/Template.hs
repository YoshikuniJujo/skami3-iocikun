module Template (
	tables, runDB, selectAll, deleteAll
	) where

import Control.Exception
import Data.Monoid
import Language.Haskell.TH

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Postgresql

import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

tables :: String -> [EntityDef] -> DecsQ
tables ma = share [mkPersist sqlSettings, mkMigrate ma]

database :: BS.ByteString
database = "host=localhost port=5432 " <>
	"user=skami3-iocikun_LOWER dbname=skami3-iocikun_LOWER"

runDB :: Migration -> ReaderT SqlBackend (ResourceT (NoLoggingT IO )) a -> IO a
runDB mg m = catch (runNoLoggingT . runResourceT . withPostgresqlConn database
		. runSqlConn $ runMigration mg >> m) $
	\(e :: SqlError) -> do
		BSC.putStrLn $ sqlErrorMsg e
		throwIO e

selectAll :: forall t m backend . (PersistEntityBackend t ~ BaseBackend backend,
		PersistEntity t, PersistQueryRead backend, MonadIO m) => 
	ReaderT backend m [Entity t]
selectAll = selectList [] []

deleteAll :: forall record m backend . (
		PersistEntityBackend record ~ BaseBackend backend,
		PersistEntity record, PersistQueryWrite backend,
		MonadIO m) =>
	ReaderT backend m ()
deleteAll = deleteWhere @_ @_ @record []
