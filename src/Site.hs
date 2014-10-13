{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app, routes
  ) where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Configurator                           as C
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Heist
import qualified Heist.Interpreted                           as I
import           Heist.Splices.BindStrict
import           Heist.Splices.Ignore
import           Prelude                                     hiding ((++))
import           Snap.Plus
import           Snap.Plus.Handlers
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.RedisDB
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Directory                            (createDirectoryIfMissing)
import qualified Text.XmlHtml                                as X

import           Application
import           FileStore

import qualified Auth.Handlers
import qualified Course.Handlers
import qualified Dependency.Handlers
import qualified Step.Handlers
import qualified Tutorial.Handlers

routes :: [(Text, AppHandler ())]
routes = [ ("tutorials",    route Tutorial.Handlers.routes)
         , ("steps",        routeResource Step.Handlers.stepResource)
         , ("dependencies", route Dependency.Handlers.routes)
         , ("courses",      route Course.Handlers.routes)
         , ("auth",         route Auth.Handlers.routes)
         , ("",             ifTop $ redirect "tutorials")
         , ("",             serveDirectory "static")
         , ("",             do modifyResponse (setResponseCode 404)
                               render "notfound")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "templates"
         mempty { hcLoadTimeSplices = defaultLoadTimeSplices,
                  hcInterpretedSplices = do "currentPath" ## pathSplice
                                            siteSplices }
    conf <- getSnapletUserConfig
    url <- liftIO (C.require conf "siteUrl")
    aPath <- liftIO (C.lookupDefault "" conf "absolutePath")
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager (aPath ++ "site_key.txt") "sess" Nothing
    p <- nestSnaplet "persistent" persistent $ initPersist (return ())
    d <- nestSnaplet "db" db pgsInit
    r <- nestSnaplet "redis" redis redisDBInitConf
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    e <- getEnvironment
    addAuthSplices h auth
    addRoutes routes
    let storePath = aPath ++ "store"
    addRoutes [("store", serveDirectory storePath)]
    liftIO $ createDirectoryIfMissing True storePath
    k <- liftIO (C.require conf "signup-key")
    return $ App h s a p d r url conf (pack e) (Directory storePath) aPath k

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
                 "siteLogoPath" ## do
                   conf' <- use conf
                   siteLogoPath' <- liftIO (C.require conf' "site-logo-path")
                   I.textSplice siteLogoPath'
                 "tutorialDefaultIconPath" ## do
                   conf' <- use conf
                   tutorialDefaultIconPath' <- liftIO (C.require conf' "tutorial-default-icon-path")
                   I.textSplice tutorialDefaultIconPath'
