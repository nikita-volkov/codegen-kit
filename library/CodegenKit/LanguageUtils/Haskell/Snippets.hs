module CodegenKit.LanguageUtils.Haskell.Snippets where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import CodegenKit.Prelude
import qualified Data.Text as Text
import qualified TextBuilder as B'

-- |
-- Multiline Haddock in the prefix position.
prefixHaddock :: Text -> B.Builder
prefixHaddock =
  mappend "-- |"
    . foldMap (mappend "\n-- ")
    . fmap fromText
    . Text.lines

-- |
-- Multiline Haddock in the prefix position followed by a line break.
prefixHaddockWithNewline :: Text -> B.Builder
prefixHaddockWithNewline =
  filtered (not . B.null) (flip mappend "\n") . prefixHaddock

-- |
-- Multiline Haddock in the suffix position.
suffixHaddock :: Text -> B.Builder
suffixHaddock =
  mappend "-- ^"
    . foldMap (mappend "\n-- ")
    . fmap fromText
    . Text.lines

-- |
-- Multiline Haddock in the suffix position preceded by a line break.
suffixHaddockWithNewline :: Text -> B.Builder
suffixHaddockWithNewline =
  filtered (not . B.null) (mappend "\n") . suffixHaddock

-- *

decimalIndexName :: Int -> B.Builder
decimalIndexName =
  B.uniline . mappend "_" . B'.decimal

alphabeticIndexName :: Int -> B.Builder
alphabeticIndexName a =
  fromString $ showIntAtBase 26 (chr . (+) 97) a ""

enumAlphabeticNames :: Int -> [B.Builder]
enumAlphabeticNames =
  fmap alphabeticIndexName . enumFromTo 0 . pred

-- *

namespace :: [Name] -> B.Builder
namespace =
  B.uniline . B'.intercalate "." . fmap Name.toUpperCamelCaseTextBuilder

moduleRef :: [Name] -> Name -> B.Builder
moduleRef nsNameList moduleName =
  B.uniline . mconcat $
    [ foldMap (flip mappend "." . Name.toUpperCamelCaseTextBuilder) nsNameList,
      Name.toUpperCamelCaseTextBuilder moduleName
    ]
