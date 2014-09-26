{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Data.Maybe
import qualified Data.Text                          as T
import           Data.Time.Clock
import           Snap.Plus                          (liftIO, route, void, with)
import           Snap.Snaplet.Auth                  (AuthUser (..))
import qualified Snap.Snaplet.Auth                  as A
import           Snap.Snaplet.PostgresqlSimple.Plus
import           System.Random                      (randomIO)
import           Test.Hspec
import           Test.Hspec.Snap

import           Application
import           Database.Persist                   (Entity (..))
import qualified Database.Persist                   as P
import           Dependency.Types
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

class Factory b a c => FactoryWith b a c t | a -> b, a -> c where
  buildWith :: FactoryArgs a x => t -> x -> SnapHspecM b a
  createWith :: FactoryArgs a x => t -> x -> Maybe UTCTime -> SnapHspecM b a
  createWith t x now = do a <- buildWith t x
                          i <- save a now
                          let newa = setId i a
                          return newa

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

instance Factory App DependencyEntity DependencyId where
  build args = do t1 <- create () Nothing
                  t2 <- create () Nothing
                  return $ toArgs args (Entity (mkKey 0) (Dependency (entityKey t1) (entityKey t2)))
  save (Entity _ d) _ = eval $ runPersist $ P.insert d
  setId k (Entity _ d) = Entity k d

data PublishedTutorial = PublishedTutorial
data DraftTutorial = DraftTutorial
data MixedTutorial = MixedTutorial

instance FactoryWith App  DependencyEntity DependencyId PublishedTutorial where
  buildWith _ args =
    do t1 <- create (setMode Published) Nothing
       t2 <- create (setMode Published) Nothing
       return $ toArgs args (Entity (mkKey 0) (Dependency (entityKey t1) (entityKey t2)))

instance FactoryWith App  DependencyEntity DependencyId DraftTutorial where
  buildWith _ args =
    do t1 <- create (setMode Draft) Nothing
       t2 <- create (setMode Draft) Nothing
       return $ toArgs args (Entity (mkKey 0) (Dependency (entityKey t1) (entityKey t2)))

instance FactoryWith App  DependencyEntity DependencyId MixedTutorial where
  buildWith _ args =
    do t1 <- create (setMode Published) Nothing
       t2 <- create (setMode Draft) Nothing
       return $ toArgs args (Entity (mkKey 0) (Dependency (entityKey t1) (entityKey t2)))

type HspecApp = SnapHspecM App

setTitle title (Entity k t) = Entity k t {tutorialTitle = title} :: TutorialEntity
setMode m (Entity k t) = Entity k t {tutorialPublish = m} :: TutorialEntity

deleteAll :: AppHandler ()
deleteAll = do runPersist $ P.deleteWhere ([] :: [P.Filter Dependency])
               runPersist $ P.deleteWhere ([] :: [P.Filter Tutorial])
               void $ execute_ "delete from snap_auth_user"


main :: IO ()
main = hspec $ do
  describe "basic tests" $ snap (route routes) app $ afterEval deleteAll $ do
    it "should redirect from index to /tutorials" $
      get "/" >>= should300To "tutorials"
    it "should render /tutorials" $
      get "/tutorials" >>= should200
  describe "api test" $ snap (route routes) app $ afterEval deleteAll $ do
    describe "/tutorial?format=json" $
      do it "should return published tutorials" $
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
    describe "/dependencies?format=json" $
      do it "should return dependencies between published tutorials" $
           do createWith PublishedTutorial () Nothing :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldHaveText "source"
         it "should not return dependencies between draft tutorials" $
           do createWith DraftTutorial () Nothing :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldNotHaveText "source"
         it "should not return dependencies between draft and published tutorials" $
           do createWith MixedTutorial () Nothing :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldNotHaveText "source"
         it "should return dependencies between draft tutorials when logged in" $ withAccount $
           do createWith DraftTutorial () Nothing :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldHaveText "source"
  describe "accounts" $ snap (route routes) app $ afterEval deleteAll $ do
    let userCount = numberQuery' "select count(*) from snap_auth_user"
    describe "signup" $
      do it "should not be able to signup without key" $
           do get "/auth/signup" >>= should404
              post "/auth/signup" (params [("signup.email.address", "a@gmail.com")
                                          ,("signup.email.confirm", "a@gmail.com")
                                          ,("signup.password", "pass")])
                >>= should404
         it "should be able to signup with proper key" $
           do get' "/auth/signup" (params [("key", "111")]) >>= should200
              post "/auth/signup" (params [("key", "111")
                                          ,("signup.email.address", "a@gmail.com")
                                          ,("signup.email.confirm", "a@gmail.com")
                                          ,("signup.password", "pass")])
                >>= should300To "/"
              eval userCount >>= shouldEqual 1


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
