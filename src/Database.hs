module Database (
  withDB,
  module Database.Persist.Sqlite,
  module Database.Schema) where

import ServerWorker
import Database.Persist.Sqlite
import Database.Schema
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource

withDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> ServerWorker a
withDB dbAction = do
  db <- askDatabase
  liftIO $ runSqlite db dbAction
