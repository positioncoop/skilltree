{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Data.Maybe
import qualified Data.Text                          as T
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

class FactoryArgs d t where
  toArgs :: t -> d -> d

instance FactoryArgs d () where
  toArgs _ = id

instance FactoryArgs d (d -> d) where
  toArgs f = f

class Factory b a c d | a -> b, a -> c, a -> d, d -> a where
  build :: d -> SnapHspecM b a
  save :: a -> SnapHspecM b c
  factoryFields :: d
  setId :: c -> a -> a
  create :: FactoryArgs d t => t -> SnapHspecM b a
  create transform = do a <- build $ (toArgs transform) factoryFields
                        i <- save a
                        let newa = setId i a
                        return newa
  reload :: a -> SnapHspecM b a
  reload = return


type TutorialFields = (Int, Int, T.Text, Maybe FilePath, Publish)

instance Factory App TutorialEntity TutorialId TutorialFields where
  factoryFields = (0, 0, "Untitled", Nothing, Draft)
  build (x,y,t,ic,pub) = return (Entity (mkKey 0) (Tutorial x y t ic pub))
  save (Entity _ t) = eval $ (runPersist $ P.insert t :: AppHandler TutorialId)
  setId k (Entity _ t) = Entity k t

instance Factory App AuthUser A.UserId (IO T.Text) where
  factoryFields = do n <- randomIO :: IO Int
                     return $ T.pack $ "user" ++ show (n `mod` 10000)
  build mlogin = do username <- liftIO mlogin
                    return $ A.defAuthUser { userLogin = username }
  save au = do mau <- eval $ with auth $ A.saveUser au
               case mau of
                 Left err -> error (show err)
                 Right au' ->
                   return (fromJust $ A.userId au')
  setId i a = a { A.userId = Just i }

type DependentFields = (SnapHspecM App TutorialEntity, SnapHspecM App TutorialEntity)

instance Factory App DependencyEntity DependencyId DependentFields where
  factoryFields = (create (), create ())
  build (mtutorial1, mtutorial2) = do
    t1 <- mtutorial1
    t2 <- mtutorial2
    return $ (Entity (mkKey 0) (Dependency (entityKey t1) (entityKey t2)))
  save (Entity _ d)  = eval $ runPersist $ P.insert d
  setId k (Entity _ d) = Entity k d

type HspecApp = SnapHspecM App

setTitle :: T.Text -> TutorialFields -> TutorialFields
setTitle title (x, y, _, icon, publish) = (x, y, title, icon, publish)
setMode :: Publish -> TutorialFields -> TutorialFields
setMode mode (x, y, title, icon, _) = (x, y, title, icon, mode)

setDependent1 :: SnapHspecM App TutorialEntity -> DependentFields -> DependentFields
setDependent1 l (_,r) = (l,r)
setDependent2 :: SnapHspecM App TutorialEntity -> DependentFields -> DependentFields
setDependent2 r (l,_) = (l,r)

setDependentModes m1 m2 = setDependent1 (create (setMode m1)) . setDependent2 (create (setMode m2))

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
           do create (setTitle "Tutorial Title" . setMode Published) :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldHaveText "Tutorial Title"
         it "should not return draft tutorials when not logged in" $
           do create (setTitle "Tutorial Title" . setMode Draft) :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldNotHaveText "Tutorial Title"
         it "should return draft tutorials when logged in" $ withAccount $
           do create (setTitle "Tutorial Title" . setMode Draft) :: HspecApp TutorialEntity
              get' "/tutorials" (params [("format", "json")])
                >>= shouldHaveText "Tutorial Title"
         it "should return multiple tutorials" $
           do create (setTitle "Tutorial #1" . setMode Published) :: HspecApp TutorialEntity
              create (setTitle "Tutorial #2" . setMode Published) :: HspecApp TutorialEntity
              p <- get' "/tutorials" (params [("format", "json")])
              shouldHaveText "Tutorial #1" p
              shouldHaveText "Tutorial #2" p
    describe "/dependencies?format=json" $
      do it "should return dependencies between published tutorials" $
           do create (setDependentModes Published Published) :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldHaveText "source"
         it "should not return dependencies between draft tutorials" $
           do create (setDependentModes Draft Draft) :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldNotHaveText "source"
         it "should not return dependencies between draft and published tutorials" $
           do create (setDependentModes Published Draft) :: HspecApp DependencyEntity
              get' "/dependencies" (params [("format", "json")])
                >>= shouldNotHaveText "source"
         it "should return dependencies between draft tutorials when logged in" $ withAccount $
           do create (setDependentModes Draft Draft) :: HspecApp DependencyEntity
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
withAccount h = do u' <- create () :: HspecApp AuthUser
                   u <- reload u'
                   loginAs u h

withAccount' :: (AuthUser -> SnapHspecM App a) -> SnapHspecM App a
withAccount' h = do u' <- create () :: HspecApp AuthUser
                    u <- reload u'
                    loginAs u (h u)
