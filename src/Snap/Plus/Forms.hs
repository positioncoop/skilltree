{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Snap.Plus.Forms where

import Control.Monad
import Control.Applicative
import Text.Digestive
import qualified Data.Text as T
import Data.Char
import Text.Digestive.Snap
import Snap.Util.FileUploads

import Application
import Snap.Plus

requiredForm :: Text -> Form Text AppHandler (Maybe a) -> Form Text AppHandler a
requiredForm msg = validate (maybe (Error msg) Success)

nameForm :: Maybe Text -> Form Text AppHandler Text
nameForm = nonEmpty . text

emailForm :: Maybe Text -> Form Text AppHandler (Maybe Text)
emailForm t =  fmap T.toLower <$>
               (fst <$> matching ((,) <$> "address" .: emailValidateSimple (optionalText t)
                                      <*> "confirm" .: emailValidateSimple (optionalText t)))
  where matching = check "Email addresses do not match."
                         (maybe True (uncurry (==)) . uncurry (liftM2 (,)))

emailFormSingle :: Maybe Text -> Form Text AppHandler (Maybe Text)
emailFormSingle t = fmap T.toLower <$> ("address" .: emailValidateSimple (optionalText t))

emailValidateSimple :: Form Text AppHandler (Maybe Text) -> Form Text AppHandler (Maybe Text)
emailValidateSimple = check "Email address not valid (missing @)." (maybe True ("@" `T.isInfixOf`))

passwordForm :: Form Text AppHandler Text
passwordForm = nonEmptyTextForm

nonEmpty :: Form Text AppHandler Text -> Form Text AppHandler Text
nonEmpty = check "Must not be blank" tNotNull

nonEmptyTextForm :: Form Text AppHandler Text
nonEmptyTextForm = nonEmpty (text Nothing)


slugForm :: Formlet Text AppHandler Text
slugForm t = T.toLower <$> check "Cannot have spaces" (not . T.isInfixOf " ") (text t)


deleteForm :: Text -> Form Text AppHandler Bool
deleteForm t = snd <$> ((,) <$> "prompt" .: text (Just t)
                            <*> "confirm" .: bool Nothing)

numericTextForm :: Form Text AppHandler Text
numericTextForm = check "Must be all numbers" (all isDigit . T.unpack) (text Nothing)

runMultipartForm :: MonadSnap m	=> Text -> Form v m a -> m (View v, Maybe a)
runMultipartForm = runFormWith (defaultSnapFormConfig { uploadPolicy = setMaximumFormInputSize tenmegs defaultUploadPolicy
                                                      , partPolicy = const $ allowWithMaximumSize tenmegs})
  where tenmegs = 10 * 1024 * 1024
