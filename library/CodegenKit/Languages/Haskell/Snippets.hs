module CodegenKit.Languages.Haskell.Snippets
  ( haddock,
    haddockWithNewline,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import CodegenKit.Prelude
import qualified Data.Text as Text
import qualified TextBuilder as B'

haddock :: Text -> B.Builder
haddock =
  mappend "-- |"
    . foldMap (mappend "\n-- ")
    . fmap fromText
    . Text.lines

-- |
-- Multiline Haddock in the prefix position.
-- I.e., appended with newline.
haddockWithNewline :: Text -> B.Builder
haddockWithNewline =
  filtered (not . B.null) (flip mappend "\n") . haddock
