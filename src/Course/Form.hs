{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Course.Form where

import Control.Lens
import Prelude hiding ((++))
import Text.Digestive
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified System.IO as IO
import qualified Control.Monad.Trans.Resource as R
import Data.Conduit.ImageSize

import Course.Types
import Application

newForm :: Form Text AppHandler Course
newForm = Course <$> "title" .: text Nothing
