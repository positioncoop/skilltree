{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Splices where

import Prelude hiding ((++))
import Heist
import Heist.Interpreted

import Tutorial.Types
import Helpers
import Application

tutorialSplice :: Tutorial -> Splices (Splice AppHandler)
tutorialSplice (Tutorial _x _y _title _iconPath) = do
  "tutorialTitle" ## textSplice _title
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y
  "tutorialIconPath" ## textSplice $ tshow $ fromMaybe "" _iconPath
