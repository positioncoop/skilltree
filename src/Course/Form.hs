{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Course.Form where

import           Control.Lens
import           Database.Persist
import           FileStore
import           Prelude                      hiding ((++))
import           Snap.Plus
import           Snap.Plus.Forms
import           Snap.Snaplet.Persistent
import           Text.Digestive

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import           Data.Conduit.ImageSize
import qualified System.IO                    as IO

import           Application
import           Course.Types

newForm :: Form Text AppHandler Course
newForm = Course <$> "title" .: text Nothing
