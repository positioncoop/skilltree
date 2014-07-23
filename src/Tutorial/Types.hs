{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls,
             LiberalTypeSynonyms, MultiParamTypeClasses, Arrows #-}

module Tutorial.Types where

import Control.Monad (void)
import Control.Applicative hiding (Const)
import Data.Text (Text)
import Data.Aeson.Types
import Data.Maybe
import Karamaan.Opaleye.Reexports
import Karamaan.Opaleye.Wire
import Karamaan.Opaleye.Table
import Karamaan.Opaleye.ShowConstant
import Karamaan.Opaleye.ExprArr (ExprArr, Expr)
import Karamaan.Opaleye.MakeExpr (makeExpr, makeJustExpr, makeMaybeExpr)
import qualified Karamaan.Opaleye.ExprArr as E
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product.Default (Default, def)
import Control.Category ((<<<))
import Control.Arrow (returnA, arr)

import Application

data Tutorial' a b c d = Tutorial' { tutorialId :: a
                                   , tutorialX :: b
                                   , tutorialY :: c
                                   , tutorialTitle :: d
                                   }
type Tutorial'' f = Tutorial' (f Int) (f Int) (f Int) (f  Text)
type Tutorial = Tutorial'' I
type TutorialSpec = Tutorial'' (Const (Wire String))
type TutorialWire = Tutorial'' Wire
type TutorialMaybeWire = Tutorial'' MaybeWire
type NewTutorial = Tutorial' (Maybe Int) Int Int Text

$(makeAdaptorAndInstance "pTutorial" ''Tutorial')

instance ToJSON Tutorial where
  toJSON (Tutorial' i x y t) = object ["id" .= i, "x" .= x, "y" .= y, "title" .= t]

tutorialsTable :: Table TutorialWire
tutorialsTable = Table "tutorial" (Tutorial' (Wire "id") (Wire "x") (Wire "y") (Wire "title"))

allTutorials :: Query TutorialWire
allTutorials = queryTable tutorialsTable

getAllTutorials :: AppHandler [Tutorial]
getAllTutorials = runO allTutorials

getTutorialById :: Int -> AppHandler (Maybe Tutorial)
getTutorialById _id = listToMaybe <$> runO (tutorialById _id)

getTutorialAtCoords :: Int -> Int -> AppHandler (Maybe Tutorial)
getTutorialAtCoords x y = listToMaybe <$> runO (tutorialsAtCoords x y)

insertTutorial :: NewTutorial -> AppHandler ()
insertTutorial tutorial@(Tutorial' _ x y t) = void $ insO tutorialsTable insertExpr
  where insertExpr :: Expr TutorialMaybeWire
        insertExpr = makeMaybeExpr tutorial { tutorialX = Just x
                                            , tutorialY = Just y
                                            , tutorialTitle = Just t
                                            }

deleteTutorialById :: Int -> AppHandler ()
deleteTutorialById _id = void $ delO tutorialsTable (chooseTutorialById _id)

updateTutorial :: Tutorial -> AppHandler ()
updateTutorial tutorial@(Tutorial' _id _x _y _title) =
  void $ updO tutorialsTable updExp (chooseTutorialById _id)
  where updExp :: ExprArr TutorialWire TutorialMaybeWire
        updExp = makeJustExpr tutorial <<< arr (const ())


tutorialsAtCoords :: Int -> Int -> Query TutorialWire
tutorialsAtCoords x y = proc () -> do tutorial <- allTutorials -< ()
                                      x' <- constant x -< ()
                                      y' <- constant y -< ()
                                      restrict <<< eq -< (tutorialX tutorial, x')
                                      restrict <<< eq -< (tutorialY tutorial, y')
                                      returnA -< tutorial

tutorialById :: Int -> Query TutorialWire
tutorialById _id = proc () -> do tutorial <- allTutorials -< ()
                                 id' <- constant _id -< ()
                                 restrict <<< eq -< (id', tutorialId tutorial)
                                 returnA -< tutorial

chooseTutorialById :: Int -> ExprArr TutorialWire (Wire Bool)
chooseTutorialById _id = proc tutorial -> do id' <- E.constant _id -< ()
                                             E.eq -< (id', tutorialId tutorial)
