{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}

module Tutorial.Publish where

import Database.Persist.TH

data Publish = Draft | Published deriving (Show, Read, Eq)
derivePersistField "Publish"
