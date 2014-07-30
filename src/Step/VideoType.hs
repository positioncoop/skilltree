{-# LANGUAGE TemplateHaskell #-}
module Step.VideoType where

import Database.Persist.TH

data VideoProvider = YouTube | Vimeo deriving (Show, Read, Eq)
derivePersistField "VideoProvider"
