{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables #-}

module Site
  ( app, routes
  ) where

import           Prelude hiding ((++))
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.RedisDB
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Heist
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
import           Heist.Splices.BindStrict
import           Heist.Splices.Ignore
import           Network.DNS.Resolver
import qualified Data.Configurator as C

import           SnapPrelude
import           Application
import           FileStore

import qualified Auth.Handlers
import qualified Tutorial.Handlers
import qualified Step.Handlers


routes :: [(ByteString, AppHandler ())]
routes = [ ("tutorials", route Tutorial.Handlers.routes)
         , ("steps",     route Step.Handlers.routeWithoutTutorial)
         , ("auth",      route Auth.Handlers.routes)
         , ("",          serveDirectory "static")
         , ("store",     serveDirectory "store")
         , ("",          render "notfound")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates"
         mempty { hcLoadTimeSplices = defaultLoadTimeSplices,
                  hcInterpretedSplices = (do "currentPath" ## pathSplice
                                             siteSplices) }
    conf <- getSnapletUserConfig
    url <- liftIO (C.require conf "siteUrl")
    absPath <- liftIO (C.lookupDefault "" conf "absolutePath")
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager (absPath ++ "site_key.txt") "sess" Nothing
    p <- nestSnaplet "persistent" persistent $ initPersist (return ())
    d <- nestSnaplet "db" db pgsInit
    r <- nestSnaplet "redis" redis redisDBInitConf
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    ns <- liftIO $ makeResolvSeed defaultResolvConf
    e <- getEnvironment
    addRoutes routes
    return $ App h s a p d r ns url (T.pack e) (Directory (absPath ++ "store"))

prefixUrlSplice :: I.Splice AppHandler
prefixUrlSplice = do node <- getParamNode
                     case X.getAttribute "url" node of
                       Nothing -> return []
                       Just u -> lift $ ifIsUrl u (return $ X.elementChildren node) (return [])

suffixUrlSplice :: I.Splice AppHandler
suffixUrlSplice = do node <- getParamNode
                     case X.getAttribute "url" node of
                       Nothing -> return []
                       Just u -> do url <- fmap (T.takeWhile (/= '?') . T.decodeUtf8 . rqURI) getRequest
                                    return $ if u `T.isSuffixOf` url
                                              then X.elementChildren node
                                              else []

pathSplice :: I.Splice AppHandler
pathSplice = do path' <- getCurrentPath
                return [X.TextNode path']

siteSplices :: Splices (I.Splice AppHandler)
siteSplices = do "prefix-url" ## prefixUrlSplice
                 "suffix-url" ## suffixUrlSplice
                 bindStrictTag ## bindStrictImpl
                 ignoreTag ## ignoreImpl
