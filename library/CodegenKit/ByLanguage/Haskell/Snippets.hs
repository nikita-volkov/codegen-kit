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

-- * --

multilineList :: [Builder] -> Builder
multilineList =
  \case
    [] -> "[]"
    a : b -> "[ " <> a <> indent 2 (foldMap (mappend ",\n") b) <> "\n]"

staticMonoid :: [Builder] -> Builder
staticMonoid = \case
  [] -> "mempty"
  [a] -> a
  a -> "mconcat " <> indent 2 ("\n" <> multilineList a)

inDoubleQuotes :: Builder -> Builder
inDoubleQuotes a = mconcat ["\"", a, "\""]

stringLiteral :: Text -> Builder
stringLiteral =
  inDoubleQuotes
    . intercalate "\\n\\\n\\"
    . fmap from
    . Text.lines
    . Text.pack
    . join
    . fmap escapeChar
    . Text.unpack
  where
    escapeChar = \case
      '\\' -> "\\\\"
      '"' -> "\\\""
      a -> [a]

multilineApplicative :: Builder -> [Builder] -> Builder
multilineApplicative construcor aps =
  case aps of
    [] -> "pure " <> construcor
    head : aps -> construcor <> indent 2 ("\n<$> " <> head <> foldMap (mappend "\n<*> ") aps)

-- * --

recordFieldsBlock ::
  -- | Fields rendered using 'recordFieldDecl'.
  [Builder] ->
  Builder
recordFieldsBlock fields =
  "{ " <> indent 2 (mconcat (intersperse ",\n" fields)) <> "\n}"

recordFieldDecl ::
  -- | Haddock.
  Builder ->
  -- | Field name.
  Builder ->
  -- | Bang.
  Builder ->
  -- | Field type.
  Builder ->
  Builder
recordFieldDecl haddock fieldName bang fieldType =
  appendNewlineIfNotNull haddock <> fieldName <> " :: " <> bang <> fieldType

appendNewlineIfNotNull :: Builder -> Builder
appendNewlineIfNotNull builder = if null builder then mempty else builder <> "\n"

newtypeRecordDecl typeName conName fieldHaddock fieldName fieldType =
  indent 2 $
    mconcat
      [ "newtype ",
        typeName,
        " = ",
        conName,
        "\n",
        recordFieldsBlock [recordFieldDecl fieldHaddock fieldName "" fieldType]
      ]

dataRecordDecl typeName conName fields =
  indent 2 $
    mconcat
      [ "data ",
        typeName,
        " = ",
        conName,
        "\n",
        recordFieldsBlock fields
      ]

enumDecl typeName options =
  indent 2 $
    mconcat
      [ "data ",
        typeName,
        "\n= ",
        intercalate "\n| " (fmap (indent 2) options)
      ]
