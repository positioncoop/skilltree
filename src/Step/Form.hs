{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Step.Form where

import Prelude hiding ((++))
import Control.Lens
import Text.Digestive
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore

import Step.Types
import Application

newForm :: Int -> Form Text AppHandler Step
newForm _tutorialId = Step _tutorialId <$> "content" .: text Nothing
                                       <*> "ordinal" .: stringRead "Must be a number" Nothing
                                       <*> "video-code" .: optionalText Nothing
                                       <*> "video-provider" .: choice [(Nothing, ""),
                                                                       (Just YouTube, "YouTube"),
                                                                       (Just Vimeo, "Vimeo")] Nothing

editForm :: Step -> Form Text AppHandler Step
editForm (Step _tutorialId _content _ordinal _videoCode _videoProvider) = Step _tutorialId <$> "content" .: text (Just _content)
                                                                 <*> "ordinal" .: stringRead "Must be a number" (Just _ordinal)
                                                                 <*> "video-code" .: optionalText _videoCode
                                                                 <*> "video-provider" .: choice [(Nothing, ""),
                                                                                                 (Just YouTube, "YouTube"),
                                                                                                 (Just Vimeo, "Vimeo")] (Just _videoProvider)
