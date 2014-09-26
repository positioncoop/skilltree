{-# LANGUAGE OverloadedStrings #-}
module Auth.Handlers where

import           Application
import           Auth.Form
import           Control.Lens
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T
import           Snap.Plus
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap

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
signupH = do tk <- getParam "key"
             k <- use signupKey
             if (tk /= Just k) then pass
               else do
                 r <- runForm "signup" signupForm
                 case r of
                   (v, Nothing) -> renderWithSplices "auth/signup" (digestiveSplices v)
                   (_, Just (email, password)) -> do with auth $ createUser email $ T.encodeUtf8 password
                                                     redirect "/"
