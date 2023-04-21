module GPTi.Aeson (jsonOpts, deriveJSON) where

import           Data.Aeson    (Options (..), defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Text.Casing   (quietSnake)

jsonOpts :: Int -> Options
jsonOpts x = defaultOptions
  { fieldLabelModifier = quietSnake . drop x
  , constructorTagModifier = quietSnake
  , omitNothingFields = True
  }
