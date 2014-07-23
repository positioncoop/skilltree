{-# LANGUAGE TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, PackageImports, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Application where

import Data.Monoid (Monoid, mappend)
import Control.Lens
import "mtl" Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple hiding (Query)
import Snap.Snaplet.Persistent
import Snap.Snaplet.RedisDB
import Database.Redis (Redis)
import qualified Database.PostgreSQL.Simple as SQL hiding (Query)
import Network.DNS.Resolver
import Control.Monad.Logger
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import Database.HaskellDB.Query (ShowConstant(..))
import Data.Text (Text, unpack)

import Data.Int (Int64)

import Karamaan.Opaleye.Reexports
import Karamaan.Opaleye.RunQuery as RQ
import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.Manipulation (AssocerE, Assocer, TableExprRunner,
                                      executeDeleteConnDef,executeInsertReturningConnDef,
                                      executeUpdateConnDef,executeInsertConnDef,
                                       TableMaybeWrapper)
import Karamaan.Opaleye.ExprArr (ExprArr, Expr)
import Karamaan.Opaleye.Table (Table(Table))

import Data.Profunctor.Product (PPOfContravariant(PPOfContravariant))
import Data.Profunctor.Product.Default (Default, def)

data App = App
     { _heist :: Snaplet (Heist App)
     , _sess :: Snaplet SessionManager
     , _persistent :: Snaplet PersistState
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

instance HasPersistPool (Handler b App) where
    getPersistPool = with persistent getPersistPool

runRedis :: Redis a -> AppHandler a
runRedis = runRedisDB redis

type AppHandler = Handler App App

(++) :: Monoid d => d -> d -> d
(++) = mappend


-- NOTE(dbp 2014-07-21): What follows is what would become part of snaplet-opaleye.
instance ShowConstant Text where
  showConstant = showConstant . unpack

type I a = a
type MaybeWire a = Maybe (Wire a)
type Const s a = s

withPgConn :: (SQL.Connection -> AppHandler a) -> AppHandler a
withPgConn f  =  do sdb <- use db
                    let pool = pgPool (view snapletValue sdb)
                    withResource pool f

runO :: Default QueryRunner a b => Query a -> AppHandler [b]
runO q = withPgConn $ \con -> liftIO $ RQ.runQuery def q con

delO :: Default TableExprRunner t a =>
        Table t -> ExprArr a (Wire Bool) -> AppHandler Int64
delO t e = withPgConn $ \con -> liftIO $ executeDeleteConnDef con t e

insO :: (Default (PPOfContravariant Assocer) t' t',
                 Default TableMaybeWrapper t t')
     => Table t -> Expr t' -> AppHandler Int64
insO t e = withPgConn $ \con -> liftIO $ executeInsertConnDef con t e

insOR :: (Default (PPOfContravariant Assocer) maybeWires maybeWires,
                Default TableMaybeWrapper wires maybeWires,
                Default TableExprRunner wires wires',
                Default (PPOfContravariant AssocerE) resultWires resultWires,
                Default QueryRunner resultWires haskells) =>
               Table wires
               -> Expr maybeWires
               -> ExprArr wires' resultWires
               -> AppHandler [haskells]
insOR t e r = withPgConn $ \con -> liftIO $ executeInsertReturningConnDef con t e r


updO ::  (Default TableExprRunner t u,
          Default (PPOfContravariant Assocer) t' t',
          Default TableMaybeWrapper t t') =>
     Table t -> ExprArr u t' -> ExprArr u (Wire Bool)
       -> AppHandler Int64
updO t e e' = withPgConn $ \con -> liftIO $ executeUpdateConnDef con t e e'
