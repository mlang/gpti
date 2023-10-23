{-# LANGUAGE LambdaCase #-}
module GPTi.Pandoc (codeBlocks, links, parseMarkdown) where

import           Data.Bifunctor                 (first)
import           Data.Default                   (def)
import           Data.Text                      (Text)
import           Text.Pandoc.Class              (runPure)
import           Text.Pandoc.Definition         (Attr, Block (CodeBlock),
                                                 Inline (Link), Pandoc)
import           Text.Pandoc.Error              (renderError)
import           Text.Pandoc.Readers.CommonMark (readCommonMark)
import           Text.Pandoc.Walk               (query)

codeBlocks :: Pandoc -> [(Attr, Text)]
codeBlocks = query $ \case
  CodeBlock attrs text -> [(attrs, text)]
  _                    -> mempty

links :: Pandoc -> [Text]
links = query $ \case
  Link _ _ (link, _) -> [link]
  _                  -> mempty

parseMarkdown :: Text -> Either Text Pandoc
parseMarkdown = first renderError . runPure . readCommonMark def
