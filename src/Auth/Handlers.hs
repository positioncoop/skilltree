{-# LANGUAGE OverloadedStrings #-}
module Auth.Handlers where

import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Snap hiding (redirect, route)
import SnapPrelude
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Auth.Form
import Application

routes :: [(Text, AppHandler ())]
routes = [("logout", logoutH)
         ,("", do isl <- with auth isLoggedIn
                  if isl
                     then redirect "/"
                     else route [("login", loginH)
                                 ,("signup", signupH)])]

logoutH :: AppHandler ()
logoutH = with auth logout >> redirect "/"

loginH :: AppHandler ()
loginH = do r <- runForm "login" loginForm
            case r of
              (v, Nothing) -> renderWithSplices "auth/login" (digestiveSplices v)
              (_, _) -> redirect "/"

signupH :: AppHandler ()
signupH = do r <- runForm "signup" signupForm
             case r of
               (v, Nothing) -> renderWithSplices "auth/signup" (digestiveSplices v)
               (_, Just (email, password)) -> do with auth $ createUser email $ T.encodeUtf8 password
                                                 redirect "/"
