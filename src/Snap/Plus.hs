{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}

module Snap.Plus ( module Snap
                 , module Control.Applicative
                 , module Data.Text
                 , (++)
                 , io
                 , tshow
                 , tNotNull
                 , readSafe
                 , getParam
                 , require
                 , require'
                 , getCurrentPath
                 , ifIsUrl
                 , matchesUrl
                 , fromMaybe
                 , redirect
                 , route
                 , addRoutes
                 ) where

import Prelude hiding ((++))
import Snap hiding (redirect, route, get, addRoutes, getParam)
import qualified Snap
import qualified Snap.Core
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Monoid (Monoid, mappend)
import Data.Maybe
import Data.Text (Text, pack, unpack, toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Snap.Snaplet.Persistent as Persistent
import qualified Database.Persist as Persistent

(++) :: Monoid d => d -> d -> d
(++) = mappend

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

tshow :: Show a => a -> Text
tshow = T.pack . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

class Paramable t where
  parseParamable :: Text -> Maybe t

instance Paramable Text where
  parseParamable = Just
instance Paramable Int where
  parseParamable = readSafe
instance (Persistent.PersistEntityBackend record) ~ backend =>
         Paramable (Persistent.KeyBackend backend record) where
  parseParamable param = Persistent.mkKey <$> readSafe param

getParam :: (MonadSnap m, Paramable t) => Text -> m t
getParam name = do param <- require $ Snap.getParam $ T.encodeUtf8 name
                   require' $ parseParamable $ T.decodeUtf8 param

require :: MonadPlus m => m (Maybe a) -> m a
require ma = do a' <- ma
                case a' of
                  Nothing -> mzero
                  Just a -> return a

require' :: MonadPlus m => Maybe a -> m a
require' = require . return

getCurrentPath :: MonadSnap m => m Text
getCurrentPath = fmap ( T.decodeUtf8 . urlEncode . T.encodeUtf8
                      . fst . T.breakOn "?" . T.decodeUtf8 . rqURI) getRequest

redirect :: MonadSnap m => Text -> m a
redirect = Snap.Core.redirect . T.encodeUtf8

route :: MonadSnap m => [(Text, m a)] -> m a
route = Snap.Core.route . map (first T.encodeUtf8)

addRoutes :: [(Text, Handler b v ())] -> Initializer b v ()
addRoutes = Snap.addRoutes . map (first T.encodeUtf8)

