{-# LANGUAGE OverloadedStrings #-}
module Auth.Handlers where

import Snap hiding (redirect)
import SnapPrelude
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Auth.Forms
import Application

authRoutes :: AppHandler ()
authRoutes = route [("logout", logoutH)
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
              (v, Nothing) -> renderWithSplices "login" (digestiveSplices v)
              (_, _) -> redirect "/"

signupH :: AppHandler ()
signupH = do r <- runForm "signup" signupForm
             case r of
               (v, Nothing) -> renderWithSplices "login" (digestiveSplices v)
               (_, _) -> redirect "/"
