{-# LANGUAGE TemplateHaskell, GADTs, QuasiQuotes,
    FlexibleInstances, DeriveDataTypeable, PackageImports,
    TypeFamilies, MultiParamTypeClasses #-}

module Application where

import Control.Lens
import "mtl" Control.Monad.State (get)
import Data.Text (Text)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Persistent
import Snap.Snaplet.RedisDB
import Snap.Snaplet.Auth
import Database.Redis (Redis)
import FileStore
import Data.Configurator.Types

data App = App
     { _heist :: Snaplet (Heist App)
     , _sess :: Snaplet SessionManager
     , _auth :: Snaplet (AuthManager App)
     , _persistent :: Snaplet PersistState
     , _db :: Snaplet Postgres
     , _redis :: Snaplet RedisDB
     , _siteUrl :: Text
     , _conf :: Config
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
