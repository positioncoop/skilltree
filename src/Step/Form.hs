{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Step.Form where

import           Data.Attoparsec.ByteString.Char8 hiding (Result)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Prelude                          hiding ((++))
import           Snap.Plus
import           Text.Digestive                   hiding (choice, string)

import           Application
import           Step.Types
import           Tutorial.Types

mkStep :: TutorialId -> Text -> Int -> Maybe (Text, VideoProvider) -> Step
mkStep t c o v = Step t c o (fst <$> v) (snd <$> v)

newForm :: TutorialId -> Form Text AppHandler Step
newForm _tutorialId = mkStep _tutorialId <$> "content" .: text Nothing
                                         <*> "ordinal" .: stringRead "Must be a number" Nothing
                                         <*> "video"   .: validate parseVideoUrl (optionalText Nothing)

editForm :: Step -> Form Text AppHandler Step
editForm (Step _tutorialId _content _ordinal _videoCode _videoProvider) =
  mkStep _tutorialId <$> "content" .: text (Just _content)
                     <*> "ordinal" .: stringRead "Must be a number" (Just _ordinal)
                     <*> "video"   .: validate parseVideoUrl (optionalText (renderVideoUrl <$> _videoCode <*> _videoProvider))

renderVideoUrl :: Text -> VideoProvider -> Text
renderVideoUrl c Vimeo = "https://vimeo.com/" ++ c
renderVideoUrl c YouTube = "https://www.youtube.com/watch?v=" ++ c

parseVideoUrl :: Maybe Text -> Result Text (Maybe (Text, VideoProvider))
parseVideoUrl Nothing = Success Nothing
parseVideoUrl (Just t) = case parseOnly parser (T.encodeUtf8 t) of
                           Left err -> Error $ "Unable to recognize video url. Can be YouTube or Vimeo - put the normal url, not the embed code. The underlying error was: " ++ (T.pack err)
                           Right v -> Success $ Just v
  where parser = parseVimeo <|> parseYoutube
        parseVimeo = do string "https" <|> string "http"
                        string "://vimeo.com/"
                        code <- many1 digit
                        return (T.pack code, Vimeo)
        parseYoutube = do string "https" <|> string "http"
                          string "://www.youtube.com/watch?v="
                          code <- many1 (notChar '&')
                          return (T.pack code, YouTube)
