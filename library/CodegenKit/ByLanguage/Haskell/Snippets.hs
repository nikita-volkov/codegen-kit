module CodegenKit.ByLanguage.Haskell.Snippets where

import Coalmine.MultilineTextBuilder
import qualified Coalmine.Name as Name
import CodegenKit.Prelude hiding (intercalate, null)
import qualified Data.Text as Text
import qualified TextBuilderDev as B'

-- |
-- Multiline Haddock in the prefix position.
prefixHaddock :: Text -> Builder
prefixHaddock =
  mappend "-- |"
    . foldMap (mappend "\n-- ")
    . fmap (from @Text)
    . Text.lines

-- |
-- Multiline Haddock in the prefix position followed by a line break.
prefixHaddockWithNewline :: Text -> Builder
prefixHaddockWithNewline =
  filtered (not . null) (flip mappend "\n") . prefixHaddock

-- |
-- Multiline Haddock in the suffix position.
suffixHaddock :: Text -> Builder
suffixHaddock =
  mappend "-- ^"
    . foldMap (mappend "\n-- ")
    . fmap (from @Text)
    . Text.lines

-- |
-- Multiline Haddock in the suffix position preceded by a line break.
suffixHaddockWithNewline :: Text -> Builder
suffixHaddockWithNewline =
  filtered (not . null) (mappend "\n") . suffixHaddock

-- * --

decimalIndexName :: Int -> Builder
decimalIndexName =
  uniline . mappend "_" . B'.decimal

alphabeticIndexName :: Int -> Builder
alphabeticIndexName a =
  fromString $ showIntAtBase 26 (chr . (+) 97) a ""

enumAlphabeticNames :: Int -> [Builder]
enumAlphabeticNames =
  fmap alphabeticIndexName . enumFromTo 0 . pred

-- * --

namespace :: [Name] -> Builder
namespace =
  uniline . B'.intercalate "." . fmap Name.toUpperCamelCaseTextBuilder

moduleRef :: [Name] -> Name -> Builder
moduleRef nsNameList moduleName =
  uniline . mconcat $
    [ foldMap (flip mappend "." . Name.toUpperCamelCaseTextBuilder) nsNameList,
      Name.toUpperCamelCaseTextBuilder moduleName
    ]
