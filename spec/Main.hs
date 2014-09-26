{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Time.Clock
import           Snap.Plus                     (liftIO, route, void, with)
import           Snap.Snaplet.Auth             (AuthUser (..))
import qualified Snap.Snaplet.Auth             as A
import           Snap.Snaplet.PostgresqlSimple
import           System.Random                 (randomIO)
import           Test.Hspec
import           Test.Hspec.Snap

import           Application
import           Database.Persist              (Entity (..))
import qualified Database.Persist              as P
import           Site
import           Snap.Snaplet.Persistent
import           Tutorial.Types

class FactoryArgs a t where
  toArgs :: t -> a -> a

instance FactoryArgs a () where
  toArgs _ = id

instance FactoryArgs a (a -> a) where
  toArgs f = f

class Factory b a c | a -> b, a -> c where
  build :: FactoryArgs a t => t -> SnapHspecM b a
  save :: a -> Maybe UTCTime -> SnapHspecM b c
  setId :: c -> a -> a
  create :: FactoryArgs a t => t -> Maybe UTCTime -> SnapHspecM b a
  create t now = do a <- build t
                    i <- save a now
                    let newa = setId i a
                    return newa
  reload :: a -> SnapHspecM b a
  reload = return

instance Factory App TutorialEntity TutorialId where
  build args = return $ toArgs args (Entity (mkKey 0) (Tutorial 0 0 "Untitled" Nothing Draft))
  save (Entity _ t) _ = eval $ runPersist $ P.insert t
  setId k (Entity _ t) = Entity k t

instance Factory App AuthUser A.UserId where
  build args = do n <- liftIO $ randomIO :: SnapHspecM App Int
                  let username = T.pack $ "user" ++ show (n `mod` 10000)
                  return $ toArgs args A.defAuthUser { userLogin = username }
  save au _ = do mau <- eval $ with auth $ A.saveUser au
                 case mau of
                   Left err -> error (show err)
                   Right au' ->
                     return (fromJust $ A.userId au')
  setId i a = a { A.userId = Just i }

type HspecApp = SnapHspecM App

deleteAll :: AppHandler ()
deleteAll = do runPersist $ P.deleteWhere ([] :: [P.Filter Tutorial])
               void $ execute_ "delete from snap_auth_user"


main :: IO ()
main = hspec $ do
  describe "basic tests" $ snap (route routes) app $ do
    it "should redirect from index to /tutorials" $
      get "/" >>= should300To "tutorials"
  describe "api test" $ snap (route routes) app $ afterEval deleteAll $
    describe "/tutorial?format=json" $
      do let setTitle title (Entity k t) = Entity k t {tutorialTitle = title} :: TutorialEntity
             setMode m (Entity k t) = Entity k t {tutorialPublish = m} :: TutorialEntity
         it "should return published tutorials" $
           do create (setTitle "Tutorial Title" . setMode Published) Nothing :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldHaveText "Tutorial Title"
         it "should not return draft tutorials when not logged in" $
           do create (setTitle "Tutorial Title" . setMode Draft) Nothing :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldNotHaveText "Tutorial Title"
         it "should return draft tutorials when logged in" $ withAccount $
           do create (setTitle "Tutorial Title" . setMode Draft) Nothing :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldHaveText "Tutorial Title"
         it "should return multiple tutorials" $
           do create (setTitle "Tutorial #1" . setMode Published) Nothing :: HspecApp TutorialEntity
              create (setTitle "Tutorial #2" . setMode Published) Nothing :: HspecApp TutorialEntity
              p <- get' "/tutorials" (params [("format", "json")])
              shouldHaveText "Tutorial #1" p
              shouldHaveText "Tutorial #2" p


loginAs :: AuthUser -> SnapHspecM App a -> SnapHspecM App a
loginAs u = modifySite'
  (\s -> do with auth $ A.forceLogin u
            s)

withAccount :: SnapHspecM App a -> SnapHspecM App a
withAccount h = do u' <- create () Nothing :: HspecApp AuthUser
                   u <- reload u'
                   loginAs u h

withAccount' :: (AuthUser -> SnapHspecM App a) -> SnapHspecM App a
withAccount' h = do u' <- create () Nothing :: HspecApp AuthUser
                    u <- reload u'
                    loginAs u (h u)
