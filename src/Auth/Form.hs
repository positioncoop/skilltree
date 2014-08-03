{-# LANGUAGE OverloadedStrings #-}
module Auth.Form where

import Control.Applicative
import Snap
import Snap.Snaplet.Auth
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Text.Digestive
import Forms
import Application

loginForm :: Form Text AppHandler (Text, Text)
loginForm = checkM "Unknown email or password" exists $
            (,) <$> "email" .: requiredForm "Enter an email address" (emailFormSingle Nothing)
                <*> "password" .: text Nothing
  where exists (email, password) = do either (const False) (const True) <$>
                                        with auth (loginByUsername email (ClearText $ T.encodeUtf8 password) False)

signupForm :: Form Text AppHandler (Text, Text)
signupForm = (,) <$> "email" .: exists (requiredForm "Enter an email address" (emailForm Nothing))
                 <*> "password" .: text Nothing
  where exists = checkM "Email already taken" $ with auth . usernameExists
