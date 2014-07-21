{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Helpers (void
               , io
               , singleQuery
               , singleQuery'
               , numberQuery
               , numberQuery'
               , idQuery
               , tshow
               , bshow
               , tNotNull
               , readSafe
               , treadSafe
               , doIfJust
               , getId
               , require
               , require'
               , getCurrentPath
               , ifIsUrl
               , matchesUrl
               , fromMaybe
               ) where

import Snap.Core
import "mtl" Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (void, join, MonadPlus, mzero)
import Data.Maybe
import Snap.Snaplet.PostgresqlSimple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

ifIsUrl :: MonadSnap m => Text -> m a -> m a -> m a
ifIsUrl u is not' = do matches <- matchesUrl u
                       if matches
                        then is
                        else not'

matchesUrl :: MonadSnap m => Text -> m Bool
matchesUrl u = do url <- fmap rqURI getRequest
                  return $ u `T.isPrefixOf` T.decodeUtf8 url

io :: MonadIO m => IO a -> m a
io = liftIO

singleQuery :: (HasPostgres m, Functor m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
singleQuery stmt attrs = fmap listToMaybe $ query stmt attrs

singleQuery' :: (HasPostgres m, Functor m, FromRow r) => Query -> m (Maybe r)
singleQuery' stmt = fmap listToMaybe $ query_ stmt

idQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m (Maybe Int)
idQuery stmt attrs = fmap (join . fmap listToMaybe . listToMaybe) $ query stmt attrs

numberQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m Int
numberQuery q attrs = fmap (head.fromJust) $ singleQuery q attrs

numberQuery' :: (HasPostgres m, Functor m) => Query -> m Int
numberQuery' q = fmap (head.fromJust) $ singleQuery' q


tshow :: Show a => a -> Text
tshow = T.pack . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

bshow :: Show a => a -> ByteString
bshow = B8.pack . show

readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

treadSafe :: Read a => Text -> Maybe a
treadSafe = readSafe . T.unpack

getId :: MonadSnap m => m Int
getId = do mi <- getParam "id"
           case fmap B8.unpack mi >>= readSafe of
             Nothing -> pass
             Just i -> return i

require :: MonadPlus m => m (Maybe a) -> m a
require ma = do a' <- ma
                case a' of
                  Nothing -> mzero
                  Just a -> return a

require' :: MonadPlus m => Maybe a -> m a
require' ma = do case ma of
                   Nothing -> mzero
                   Just a -> return a

doIfJust :: Maybe a -> b -> (a -> b) -> b
doIfJust Nothing  b f = b
doIfJust (Just a) _ f = f a

getCurrentPath :: MonadSnap m => m Text
getCurrentPath = fmap ( T.decodeUtf8 . urlEncode . T.encodeUtf8
                      . fst . T.breakOn "?" . T.decodeUtf8 . rqURI) getRequest
