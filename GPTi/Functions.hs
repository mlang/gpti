{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GPTi.Functions (
  Function(..)
, getChatFunctions, findFunction
, haskell, localTime, metar
) where

import           Control.Lens                 ((?~), (^.))
import           Control.Monad.Catch          (displayException)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Aeson                   (FromJSON (..), Object,
                                               ToJSON (..),
                                               Value (Object, String), object,
                                               withText, (.:), (.=))
import           Data.Aeson.Key               (Key)
import qualified Data.Aeson.Key               as Key
import qualified Data.Aeson.KeyMap            as Object
import           Data.Aeson.Types             (Pair, Parser, parseEither)
import qualified Data.ByteString.Lazy.Char8   as ByteString (toStrict)
import           Data.Char                    (toUpper)
import           Data.Foldable                (toList)
import           Data.Set                     (Set)
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Time                    (getZonedTime)
import           Language.Haskell.Interpreter (ImportList (..),
                                               ModuleImport (..),
                                               ModuleQualification (..), eval,
                                               runInterpreter, setImportsF)
import qualified Network.Wreq                 as HTTP
import           OpenAI.Client                (ChatFunction (..))

type Schema = Object

-- | Represents a function with its name, description and argument schema.
data Function = MkFunction Text Text Schema (Object -> Parser (IO Text))

instance Eq Function where
  MkFunction a _ _ _ == MkFunction b _ _ _ = a == b

instance Ord Function where
  MkFunction a _ _ _ `compare` MkFunction b _ _ _ = a `compare` b

getChatFunctions :: Set Function -> Maybe [ChatFunction]
getChatFunctions = mkMaybe . map chatFunc . toList where
  chatFunc (MkFunction n d s _) = ChatFunction n d $ Object s
  mkMaybe [] = Nothing
  mkMaybe xs = Just xs

findFunction :: MonadIO m => Text -> Set Function -> Maybe (Object -> m Text)
findFunction name = go . toList where
  go (MkFunction n _ _ p : ys)
    | n == name = Just $ either (pure . fromString) liftIO . parseEither p
    | otherwise = go ys
  go [] = Nothing

objectType :: [Pair] -> [Pair] -> Schema
objectType props extra = Object.fromList $
  [ "type" .= String "object"
  , "properties" .= object props
  ] ++ extra

nullary :: Text -> Text -> IO Text -> Function
nullary name description =
  MkFunction name description (objectType [] []) . const . pure

stringType :: Text -> Schema
stringType description = Object.fromList
  [ "type" .= String "string"
  , "description" .= description
  ]

unary :: FromJSON a
      => Text -> Text -> (Key, Schema) -> (a -> IO Text) -> Function
unary name description (arg, schema) action =
  MkFunction name description
  (objectType [arg .= schema] ["required" .= [Key.toText arg]]) $
  \args -> action <$> args .: arg

haskell :: Function
haskell = unary "haskell"
  "Evaluate a Haskell expression"
  ("expr", stringType "Pure Haskell expression to evaluate") $
  \expr -> do
    res <- runInterpreter $ do
      setImportsF [ ModuleImport "Prelude" NotQualified NoImportList ]
      eval $ Text.unpack expr
    pure . either (fromString . displayException) fromString $ res

localTime :: Function
localTime = nullary "localTime" "Get the current local time of user" $
  fromString . show <$> getZonedTime

metar :: Function
metar = unary "metar"
  "Retrieve METAR weather information for a particular airport"
  ("icao", stringType "ICAO Airport code") $
  \icao -> do
    let url = "https://tgftp.nws.noaa.gov/data/observations/metar/stations/" <>
              map toUpper (Text.unpack . Text.strip $ icao) <>
              ".TXT"
    let opts = HTTP.checkResponse ?~ (\_ _ -> pure ()) $ HTTP.defaults
    r <- HTTP.getWith opts url
    let status = r ^. HTTP.responseStatus
    pure . Text.decodeUtf8 $ case status ^. HTTP.statusCode of
      200 -> ByteString.toStrict $ r ^. HTTP.responseBody
      _   -> status ^. HTTP.statusMessage

instance FromJSON Function where
  parseJSON = withText "Function" $ \case
    "haskell"   -> pure haskell
    "localTime" -> pure localTime
    "metar"     -> pure metar
    _           -> fail "Unknown function"

instance ToJSON Function where
  toJSON (MkFunction name _ _ _) = String name
