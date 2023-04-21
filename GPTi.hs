{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module GPTi (repl) where

import           Control.Applicative            ((<|>))
import           Control.Lens                   (assign, use, uses, (.=), (.~),
                                                 (<>=), (<>~), (?=), (^.), (^?))
import           Control.Lens.TH                (abbreviatedFields,
                                                 makeLensesWith)
import           Control.Monad                  (unless)
import           Control.Monad.Catch            (MonadCatch (..),
                                                 MonadMask (..))
import           Control.Monad.Extra            (ifM, whenM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader (..), asks,
                                                 runReaderT)
import           Control.Monad.State            (MonadState (..), execStateT,
                                                 gets)
import qualified Data.Aeson                     as Aeson
import           Data.Aeson.Lens                (_String, key)
import qualified Data.ByteString.Lazy           as ByteString (fromStrict)
import qualified Data.ByteString.Lazy.Char8     as ByteString (toStrict, unpack)
import           Data.Char                      (isSpace)
import           Data.Coerce                    (coerce)
import           Data.Foldable                  (traverse_)
import           Data.Functor                   ((<&>))
import           Data.List                      (dropWhileEnd)
import           Data.Maybe                     (isJust)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Text.IO                   as Text
import qualified Data.Vector                    as Vector
import qualified Data.Yaml.Aeson                as YAML
import           GPTi.Aeson
import           GPTi.Functions                 (Function (..), findFunction,
                                                 getChatFunctions, haskell,
                                                 localTime, metar)
import           GPTi.LanguageDetection         (declareLanguage, parseLanguage)
import           GPTi.Pandoc
import           Network.HTTP.Client            (ManagerSettings (..),
                                                 responseTimeoutMicro)
import           Network.HTTP.Client.TLS        (newTlsManagerWith,
                                                 tlsManagerSettings)
import           OpenAI.Client                  (AudioTranscriptionRequest (..),
                                                 ChatCompletionRequest (..),
                                                 ClientError (..), ModelId (..),
                                                 OpenAIClient, completeChat,
                                                 createTranscription,
                                                 defaultChatCompletionRequest,
                                                 makeOpenAIClient)
import           OpenAI.Lens                    (HasModel (model),
                                                 HasTemperature (temperature),
                                                 choices, functions, message,
                                                 messages, stop, text, topP)
import           OpenAI.PatternSynonyms
import           Servant.Client.Core.Response   (responseBody)
import           System.Console.Repline         (CompleterStyle (Prefix),
                                                 CompletionFunc,
                                                 ExitDecision (Exit),
                                                 MultiLine (..), ReplOpts (..),
                                                 dontCrash, evalReplOpts,
                                                 fileCompleter, listCompleter)
import           System.Directory               (doesFileExist)
import           System.Environment             (getEnv, lookupEnv)
import           System.Environment.XDG.BaseDir (getUserConfigFile)
import           System.IO.Temp                 (emptySystemTempFile)
import           System.Process.Typed           (StreamSpec,
                                                 StreamType (STInput),
                                                 byteStringInput, proc,
                                                 readProcessStdout_,
                                                 runProcess_, setStdin)
import           Text.ANSI                      (bold, red)
import           Text.Read                      (readMaybe)

data Transcription = Transcription {
  tModel       :: ModelId
, tPrompt      :: Maybe Text
, tTemperature :: Maybe Double
, tLanguage    :: Maybe Text
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''Transcription
$(deriveJSON (jsonOpts 1) ''Transcription)

data Config = MkConfig {
  cRecorder      :: Maybe [String]
, cSpeaker       :: Maybe [String]
, cChat          :: ChatCompletionRequest
, cChatFunctions :: Set Function
, cTranscription :: Transcription
}

makeLensesWith abbreviatedFields ''Config
$(deriveJSON (jsonOpts 1) ''Config)

chatModel, transcriptionModel :: ModelId
chatModel = coerce $ Text.pack "gpt-4"
transcriptionModel = coerce $ Text.pack "whisper-1"

defaultConfig :: Config
defaultConfig = MkConfig{..} where
  cRecorder = Just ["ffmpeg"]
  cSpeaker = Just ["espeak-ng"]
  cChat = defaultChatCompletionRequest chatModel []
  cChatFunctions = Set.fromList [haskell, localTime, metar]
  cTranscription = Transcription transcriptionModel Nothing Nothing Nothing

userConfig :: MonadIO m => m Config
userConfig = liftIO $ do
  fp <- getUserConfigFile "gpti" "config.yaml"
  exists <- doesFileExist fp
  if exists
    then YAML.decodeFileEither fp >>= \case
      Left e -> do
        putStrLn $ YAML.prettyPrintParseException e
        pure defaultConfig
      Right cfg -> pure cfg
    else pure defaultConfig

data ReplState = MkReplState {
  rsRequest   :: ChatCompletionRequest
, rsConfig    :: Config
, rsDebug     :: Bool
, rsSeenLinks :: [Text]
}

makeLensesWith abbreviatedFields ''ReplState

submit :: ( MonadCatch m
          , MonadIO m
          , MonadReader OpenAIClient m
          , MonadState ReplState m
          )
       => Text -> m ()
submit input = send $ UserMessage input where
  send msg = do
    complete <- asks completeChat
    fs <- uses (config.chatFunctions) getChatFunctions
    this <- uses request $ (messages <>~ [msg])
                         . (functions .~ fs)
    complete this >>= either printClientError (handleResponse msg)
  handleResponse msg response = case response ^. choices of
    [] -> pure ()
    c:_ -> do
      quiet <- not <$> use debug
      let reply = c ^. message
      case reply of
        FunctionCallMessage name args ->
          uses (config.chatFunctions) (findFunction name) >>= \case
            Just f -> do
              output <- f args
              unless quiet $ do
                putTextLn $
                  name <>
                  Text.decodeUtf8 (ByteString.toStrict (Aeson.encode args)) <>
                  " -> " <> output
              request.messages <>= [msg, reply]
              send $ FunctionResultMessage name output
            Nothing -> putTextLn . red $ "ERROR: Model tried to call unknown function '" <> name <> "'"
        AssistantMessage content -> do
          case parseLanguage content of
            Just (lang, txt) -> do
              sniff txt
              putTextLn $ bold txt
              speak lang txt
            Nothing -> do
              sniff content
              putTextLn . bold $ content
          request.messages <>= [msg, reply]
        _ -> pure ()
  sniff = either (putTextLn . red) (dontCrash . go) . parseMarkdown where
    go doc = do
      seenLinks <>= links doc
      traverse_ handleCodeBlock . codeBlocks $ doc

printClientError :: MonadIO m => ClientError -> m ()
printClientError (FailureResponse _ resp) = liftIO $
  case responseBody resp ^? key "error" . key "message" . _String of
    Just e  -> putTextLn . red $ "ERROR: " <> e
    Nothing -> print resp
printClientError e = liftIO . print $ e

repl :: (MonadIO m, MonadMask m) => m ChatCompletionRequest
repl = replWith =<< userConfig

replWith :: (MonadMask m, MonadIO m)
         => Config -> m ChatCompletionRequest
replWith cfg = do
  r <- flip runReaderT <$> openAI
  let s = flip execStateT $ MkReplState (cfg ^. chat) cfg True []
  fmap rsRequest . s . r $ do
    let banner SingleLine = gets $ (++ "> ") . Text.unpack . unModelId . (^. request . model)
        banner MultiLine  = pure "| "

        command = submit . fromString

        options = [ ("hear", const cmdHear)
                  , ("visit", cmdVisit)
                  , ("new", cmdNew)
                  , ("quote", cmdQuote . words)
                  , ("reset", cmdNew)
                  , ("set", cmdSet . words)
                  , ("submit", cmdSubmit . words)
                  , ("talk", cmdSpeak)
                  , ("transcribe", cmdTranscribe)
                  ]

        prefix = Just ':'

        multilineCommand = Just "|"

        tabComplete = Prefix fileCompleter [ (":quote", fileCompleter)
                                           , (":set", completeVariables)
                                           , (":submit", fileCompleter)
                                           , (":transcribe", fileCompleter)
                                           , (":visit", completeLinks)
                                           , (":", completeCommands)
                                           ] where
          completeVariables = listCompleter ["model", "temperature", "top_p"]
          completeCommands = listCompleter $ (':':) . fst <$> options
          completeLinks x = do
            ls <- use seenLinks
            listCompleter (map Text.unpack ls) x

        initialiser = pure ()

        finaliser = pure Exit

    evalReplOpts ReplOpts{..}



missingArgument, tooManyArguments :: MonadIO m => m ()
missingArgument = putTextLn . red $ "Missing argument"
tooManyArguments = putTextLn . red $ "Too many arguments"

cmdNew :: (MonadState ReplState m) => String -> m ()
cmdNew _ = assign request =<< use (config.chat)

cmdQuote :: (MonadCatch m, MonadIO m, MonadState ReplState m) => [String] -> m ()
cmdQuote [] = missingArgument
cmdQuote [fp] = dontCrash $ do
  msg <- UserMessage . ("```\n" <>) . (<> "\n```") <$> liftIO (Text.readFile fp)
  request.messages <>= pure msg
cmdQuote _ = tooManyArguments

cmdSubmit :: (MonadCatch m, MonadReader OpenAIClient m, MonadState ReplState m, MonadIO m) => [String] -> m ()
cmdSubmit []   = missingArgument
cmdSubmit [fp] = dontCrash $ submit =<< liftIO (Text.readFile fp)
cmdSubmit _    = tooManyArguments

cmdSpeak :: MonadState ReplState m => String -> m ()
cmdSpeak _ = request.messages <>= pure declareLanguage

cmdSet :: (MonadState ReplState m, MonadIO m) => [String] -> m ()
cmdSet [] = traverse_ cmdSet $ pure <$> ["debug", "model", "temperature", "top_p"]
cmdSet [var] = do
  val <- case var of
    "debug"       -> uses debug $ Just . show
    "model"       -> uses (request.model) $ Just . Text.unpack . unModelId
    "temperature" -> uses (request.temperature) $ Just . maybe "1.0" show
    "top_p"       -> uses (request.topP) $ Just . maybe "1.0" show
    _             -> pure Nothing
  case val of
    Nothing   -> liftIO . putStrLn $ "Variable '" ++ var ++ "' does not exist"
    Just val' -> liftIO . putStrLn $ var ++ " = " ++ val'
cmdSet ["debug", val] = case readMaybe val of
  Just bool -> debug .= bool
  Nothing   -> putTextLn . red $ "Invalid boolean"
cmdSet ["model", val] = request.model .= ModelId (Text.strip . fromString $ val)
cmdSet ("stop" : xs) = request.stop ?= Vector.fromList (Text.pack <$> xs)
cmdSet ["temperature", val] = case readMaybe val of
  Just temp -> request.temperature .= Just temp
  Nothing -> putTextLn . red $ "Invalid temperature value.  Should be a number between 0.0 and 2.0."
cmdSet ["top_p", val] = case readMaybe val of
  Just top_p -> request . topP .= Just top_p
  Nothing -> putTextLn . red $ "Invalid top_p value.  Should be a number between 0.0 and 2.0."
cmdSet [var, _] = liftIO . putStrLn $ "Unknown parameter:" ++ var
cmdSet _ = tooManyArguments

apiKey :: MonadIO m => m Text
apiKey = fmap fromString . liftIO $ password_store <|> env where
  password_store = do
    pass <- getEnv "OPENAI_API_KEY_PASS_NAME" <&> \n -> proc "pass" ["show", n]
    dropWhileEnd isSpace . ByteString.unpack <$> readProcessStdout_ pass
  env = getEnv "OPENAI_API_KEY"

openAI :: MonadIO m => m OpenAIClient
openAI = do
  let settings = tlsManagerSettings {
                   managerResponseTimeout = responseTimeoutMicro 120000000
                 }
  makeOpenAIClient <$> apiKey <*> newTlsManagerWith settings <*> pure 3

textInput :: Text -> StreamSpec 'STInput ()
textInput = byteStringInput . ByteString.fromStrict . Text.encodeUtf8

speak :: (MonadCatch m, MonadIO m, MonadState ReplState m) => String -> Text -> m ()
speak lang txt = use (config.speaker) >>= \case
  Nothing -> putTextLn . red $ "No speech synthesis program configured"
  Just ("espeak-ng" : args) -> do
    let cfg = setStdin (textInput txt) $
              proc "espeak-ng" $ args ++ ["--stdin", "-v", lang]
    dontCrash $ runProcess_ cfg
  Just args -> unknownProgram "speech synthesis" args

cmdHear :: (MonadCatch m, MonadState ReplState m, MonadReader OpenAIClient m, MonadIO m) => m ()
cmdHear = do
  mp3 <- use (config.recorder) >>= \case
    Nothing -> do
      putTextLn "No recording program configured"
      pure Nothing
    Just ("ffmpeg":args) -> do
      fp <- liftIO $ emptySystemTempFile "audio.mp3"
      let cfg = proc "ffmpeg" (["-loglevel", "quiet"] ++ args ++ ["-ac", "1", "-b:a", "128k", "-y", fp])
      putTextLn "[Type 'q' to stop recording]"
      dontCrash $ runProcess_ cfg
      pure $ Just fp
    Just args -> do
      unknownProgram "recording" args
      pure Nothing
  case mp3 of
    Nothing -> pure ()
    Just fp -> transcribe fp >>= \case
      Left e    -> printClientError e
      Right txt -> putTextLn txt >> submit txt

cmdVisit :: MonadIO m => String -> m ()
cmdVisit link = runProcess_ =<< ifM inTmux newWin normal where
  newWin = pure $ proc "tmux" ["new-window", "sensible-browser", stripString link]
  normal = pure $ proc "sensible-browser" [stripString link]

stripString :: String -> String
stripString = Text.unpack . Text.strip . fromString

unknownProgram :: MonadIO m => Text -> [String] -> m ()
unknownProgram kind args = putTextLn . red $
  "Unknown " <> kind <> " program: " <> fromString (show args)

transcribe :: (MonadIO m, MonadReader OpenAIClient m, MonadState ReplState m)
           => FilePath -> m (Either ClientError Text)
transcribe fp = do
  create <- asks createTranscription
  atr <- AudioTranscriptionRequest fp <$> use (config.transcription.model)
                                      <*> use (config.transcription.prompt)
                                      <*> pure Nothing -- responseFormat
                                      <*> use (config.transcription.temperature)
                                      <*> use (config.transcription.language)
  fmap (^. text) <$> create atr

cmdTranscribe :: (MonadReader OpenAIClient m, MonadState ReplState m, MonadIO m)
              => FilePath -> m ()
cmdTranscribe fp = transcribe (stripString fp) >>= \case
  Left e    -> printClientError e
  Right txt -> putTextLn $ bold txt

inTmux :: MonadIO m => m Bool
inTmux = isJust <$> liftIO (lookupEnv "TMUX")

whenTmux :: MonadIO m => m () -> m ()
whenTmux = whenM inTmux

handleCodeBlock :: MonadIO m => ((Text, [Text], [(Text, Text)]), Text) -> m ()
handleCodeBlock (_, code) = whenTmux $ pipeToTmux ["load-buffer", "-"] code

pipeToTmux :: MonadIO m => [String] -> Text -> m ()
pipeToTmux args = runProcess_ . flip setStdin (proc "tmux" args) . textInput

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . Text.putStrLn
