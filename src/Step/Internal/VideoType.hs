{-# LANGUAGE TemplateHaskell #-}
module Step.Internal.VideoType where

import Database.Persist.TH

data VideoProvider = YouTube | Vimeo deriving (Show, Read, Eq)
derivePersistField "VideoProvider"
