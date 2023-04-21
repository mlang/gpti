{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module OpenAI.PatternSynonyms
( pattern UserMessage, pattern AssistantMessage, pattern SystemMessage
, pattern FunctionCallMessage, pattern FunctionResultMessage
) where

import           Data.Aeson    (Object, Value (Object))
import           Data.Text     (Text)
import           OpenAI.Client (ChatFunctionCall (..), ChatMessage (..))

pattern UserMessage, AssistantMessage, SystemMessage :: Text -> ChatMessage
pattern UserMessage content = ChatMessage
  { chmRole = "user"
  , chmContent = Just content
  , chmFunctionCall = Nothing
  , chmName = Nothing
  }
pattern AssistantMessage content = ChatMessage
  { chmRole = "assistant"
  , chmContent = Just content
  , chmFunctionCall = Nothing
  , chmName = Nothing
  }
pattern SystemMessage content = ChatMessage
  { chmRole = "system"
  , chmContent = Just content
  , chmFunctionCall = Nothing
  , chmName = Nothing
  }

pattern FunctionCallMessage :: Text -> Object -> ChatMessage
pattern FunctionCallMessage name arguments = ChatMessage
  { chmRole = "assistant"
  , chmContent = Nothing
  , chmFunctionCall = Just (ChatFunctionCall { chfcName = name
                                             , chfcArguments = Object arguments
                                             })
  , chmName = Nothing
  }

pattern FunctionResultMessage :: Text -> Text -> ChatMessage
pattern FunctionResultMessage name result = ChatMessage
  { chmRole = "function"
  , chmContent = Just result
  , chmFunctionCall = Nothing
  , chmName = Just name
  }
