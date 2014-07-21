{-# LANGUAGE TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, PackageImports, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses #-}

module Application where

import Control.Lens
import "mtl" Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.RedisDB
import Database.Redis (Redis)
import Network.DNS.Resolver
import Database.Groundhog hiding (get)
import Database.Groundhog.Core hiding (get)
import Database.Groundhog.Postgresql hiding (get)
import Control.Monad.Logger
import Data.Pool
import Database.PostgreSQL.Simple (Connection)


data App = App
     { _heist :: Snaplet (Heist App)
     , _sess :: Snaplet SessionManager
     , _db :: Snaplet Postgres
     , _redis :: Snaplet RedisDB
     , _dns :: ResolvSeed
     , _siteUrl :: Text
     , _env :: Text
     }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get


instance ConnectionManager Connection Postgresql where
  withConn f = withConn f . Postgresql
  withConnNoTransaction f = withConnNoTransaction f . Postgresql

runGH :: DbPersist Postgresql (NoLoggingT IO) a
      -> AppHandler a
runGH f = do cm <- use db
             let pool = pgPool (view snapletValue cm)
             withResource pool $
               \con -> liftIO $ runDbConn f con

runRedis :: Redis a -> AppHandler a
runRedis = runRedisDB redis

type AppHandler = Handler App App
