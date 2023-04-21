{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GPTi.LanguageDetection (declareLanguage, parseLanguage) where

import           Control.Monad          (void)
import           Data.FileEmbed         (embedStringFile)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Void              (Void)
import           OpenAI.Client          (ChatMessage)
import           OpenAI.PatternSynonyms
import           Text.Megaparsec
import           Text.Megaparsec.Char

declareLanguage :: ChatMessage
declareLanguage = SystemMessage $(embedStringFile "prompts/declare-language.txt")

parseLanguage :: Text -> Maybe (String, Text)
parseLanguage = parseMaybe language where
  language :: Parsec Void Text (String, Text)
  language = do
    void . string $ "Language: "
    lang <- Text.unpack . Text.strip . Text.pack <$> manyTill anySingle newline
    void newline
    txt <- takeRest
    pure (lang, txt)
