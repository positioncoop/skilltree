{-# LANGUAGE OverloadedStrings #-}
module Auth.Forms where

import Data.Text (Text)
import Text.Digestive
import Forms
import Application

loginForm :: Form Text AppHandler (Text, Text)
loginForm = checkM "Unknown email or password" exists $
            (,) <$> "email" .: emailFormSingle Nothing
                <*> "password" .: text
  where exists (email, password) = do either (const False) (const True) <$>
                                        with auth $ loginByUsername email (ClearText password)

signupForm :: Form Text AppHandler (Text, Text)
signupForm = checkM "Email already taken" exists $
            (,) <$> "email" .: emailForm Nothing
                <*> "password" .: text
  where exists (email, password) = with auth $ usernameExists email
