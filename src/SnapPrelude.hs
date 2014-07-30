{-# LANGUAGE OverloadedStrings, PackageImports #-}

module SnapPrelude (void
               , io
               , tshow
               , tNotNull
               , readSafe
               , getId
               , require
               , require'
               , getCurrentPath
               , ifIsUrl
               , matchesUrl
               , fromMaybe
               , redirect
               ) where

import Snap.Core hiding (redirect)
import qualified Snap.Core
import "mtl" Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (void, MonadPlus, mzero)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

getId :: MonadSnap m => m Int
getId = do mi <- getParam "id"
           case fmap T.decodeUtf8 mi >>= readSafe of
             Nothing -> pass
             Just i -> return i

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
