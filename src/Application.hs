{-# LANGUAGE TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, PackageImports, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses #-}

module Application where

import Data.Monoid (Monoid, mappend)
import Control.Lens
import "mtl" Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Persistent
import Snap.Snaplet.RedisDB
import Snap.Snaplet.Auth
import Database.Redis (Redis)
import Network.DNS.Resolver
import Control.Monad.Logger
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import FileStore

data App = App
     { _heist :: Snaplet (Heist App)
     , _sess :: Snaplet SessionManager
     , _auth :: Snaplet (AuthManager App)
     , _persistent :: Snaplet PersistState
     , _db :: Snaplet Postgres
     , _redis :: Snaplet RedisDB
     , _dns :: ResolvSeed
     , _siteUrl :: Text
     , _env :: Text
     , _filestore :: FileStore
     }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get

instance HasPersistPool (Handler b App) where
    getPersistPool = with persistent getPersistPool

runRedis :: Redis a -> AppHandler a
runRedis = runRedisDB redis

type AppHandler = Handler App App
