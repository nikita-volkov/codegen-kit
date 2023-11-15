module CodegenKit.HaskellPackage.Contexts.GroupingCode where

import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Code qualified as Code
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as LegacyExp
import Data.Text qualified as Text

toGroupedCode :: GroupingCode -> Code.Code
toGroupedCode x =
  if x.needsGrouping
    then
      if x.isMultiline
        then "( " <> Code.indent 2 x.content <> "\n)"
        else "(" <> x.content <> ")"
    else x.content

toUngroupedCode :: GroupingCode -> Code.Code
toUngroupedCode x =
  x.content

data GroupingCode = GroupingCode
  { needsGrouping :: Bool,
    isMultiline :: Bool,
    content :: Code.Code
  }

fromLegacyExp :: LegacyExp.Exp -> GroupingCode
fromLegacyExp legacyExp =
  GroupingCode
    { needsGrouping = legacyExp.needsGrouping,
      isMultiline = legacyExp.isMultiline,
      content = Code.splice legacyExp.content
    }

localSymbol :: Text -> GroupingCode
localSymbol text =
  GroupingCode
    { needsGrouping = False,
      isMultiline = False,
      content = Code.splice (from text)
    }

-- | Produce code with a symbol reference that is determined based on the imports and requirements for them.
importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  GroupingCode
importedSymbol moduleRef memberName =
  GroupingCode
    { needsGrouping = False,
      isMultiline = False,
      content = Code.importedSymbol moduleRef memberName
    }

chainApplication :: GroupingCode -> [GroupingCode] -> GroupingCode
chainApplication function params =
  if all (not . (.isMultiline)) params && not function.isMultiline
    then
      GroupingCode
        { needsGrouping = True,
          isMultiline = False,
          content =
            toUngroupedCode function
              <> foldMap (mappend " " . toGroupedCode) params
        }
    else
      GroupingCode
        { needsGrouping = True,
          isMultiline = False,
          content =
            toGroupedCode function
              <> Code.indent 2 (foldMap (mappend "\n" . toGroupedCode) params)
        }

binApplication :: GroupingCode -> GroupingCode -> GroupingCode
binApplication function param =
  if function.isMultiline && param.isMultiline
    then
      GroupingCode
        { needsGrouping = True,
          isMultiline = True,
          content =
            function.content <> Code.indent 2 ("\n" <> toGroupedCode param)
        }
    else
      GroupingCode
        { needsGrouping = True,
          isMultiline = False,
          content =
            function.content <> " " <> toGroupedCode param
        }

stringLiteral :: Text -> GroupingCode
stringLiteral text =
  GroupingCode False isMultiline code
  where
    isMultiline =
      case processedLines of
        [] -> False
        [_] -> False
        _ -> True
    code =
      Code.splice splice
    splice =
      mconcat
        [ "\"",
          Splice.intercalate "\\n\\\n\\" processedLines,
          "\""
        ]
    processedLines =
      fmap (from @Text)
        . Text.lines
        . Text.pack
        . join
        . fmap escapeChar
        . Text.unpack
        $ text
      where
        escapeChar = \case
          '\\' -> "\\\\"
          '"' -> "\\\""
          a -> [a]
