{-# LANGUAGE OverloadedStrings #-}
module Auth.Form where

import Control.Applicative
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Auth
import qualified Data.Text.Encoding as T
import Text.Digestive
import Application

loginForm :: Form Text AppHandler (Text, Text)
loginForm = checkM "Unknown email or password" exists $
            (,) <$> "email" .: requiredForm "Enter an email address" (emailFormSingle Nothing)
                <*> "password" .: text Nothing
  where exists (email, password) = either (const False) (const True) <$>
                                   with auth (loginByUsername email (ClearText $ T.encodeUtf8 password) False)

signupForm :: Form Text AppHandler (Text, Text)
signupForm = (,) <$> "email" .: exists (requiredForm "Enter an email address" (emailForm Nothing))
                 <*> "password" .: text Nothing
  where exists = checkM "Email already taken" $ with auth . fmap not . usernameExists
