{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Form where

import Control.Lens
import Prelude hiding ((++))
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive
import Snap
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore
import Forms

import Step.Types
import Application

newForm :: Int -> Form Text AppHandler Step
newForm _tutorialId = Step _tutorialId <$> "content" .: text Nothing
                                       <*> "ordinal" .: stringRead "Must be a number" Nothing

editForm :: Step -> Form Text AppHandler Step
editForm (Step _tutorialId _content _ordinal) = Step _tutorialId <$> "content" .: text (Just _content)
                                                                 <*> "ordinal" .: stringRead "Must be a number" (Just _ordinal)
