{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module OpenAI.Lens (
  HasArguments(arguments)
, HasChoices(choices)
, HasCreated(created)
, HasContent(content)
, HasFinishReason(finishReason)
, HasFrequencyPenalty(frequencyPenalty)
, HasFunctions(functions)
, HasFunctionCall(functionCall)
, HasId(id)
, HasIndex(index)
, HasLogitBias(logitBias)
, HasMaxTokens(maxTokens)
, HasMessage(message)
, HasMessages(messages)
, HasModel(model)
, HasN(n)
, HasName(name)
, HasObject(object)
, HasPresencePenalty(presencePenalty)
, HasRole(role)
, HasStop(stop)
, HasStream(stream)
, HasTemperature(temperature)
, HasText(text)
, HasTopP(topP)
, HasUsage(usage)
, HasUser(user)
) where

import           Control.Lens.TH (abbreviatedFields, makeLensesWith)
import           OpenAI.Client

makeLensesWith abbreviatedFields ''AudioResponseData
makeLensesWith abbreviatedFields ''ChatChoice
makeLensesWith abbreviatedFields ''ChatCompletionRequest
makeLensesWith abbreviatedFields ''ChatFunctionCall
makeLensesWith abbreviatedFields ''ChatMessage
makeLensesWith abbreviatedFields ''ChatResponse
