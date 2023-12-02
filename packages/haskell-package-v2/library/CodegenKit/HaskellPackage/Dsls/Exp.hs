-- | General expression printer DSL managing metadata about dependencies, imports and extensions.
--
-- Serves as the basis for Type and Value expression DSLs.
module CodegenKit.HaskellPackage.Dsls.Exp where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeGrouping qualified as CodeGrouping
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.ComonadicContexts.Code qualified as Code
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as LegacyExp
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import TextBuilderDev qualified as UnilineSplice

toGroupedCodeSplice :: Exp -> Code.Context Splice
toGroupedCodeSplice x =
  if x.needsGrouping
    then
      if x.isMultiline
        then "( " <> fmap (Splice.indent 2) x.content <> "\n)"
        else "(" <> x.content <> ")"
    else x.content

toUngroupedCodeSplice :: Exp -> Code.Context Splice
toUngroupedCodeSplice x =
  x.content

data Exp = Exp
  { needsGrouping :: Bool,
    isMultiline :: Bool,
    content :: Code.Context Splice
  }

localSymbol :: Text -> Exp
localSymbol text =
  Exp
    { needsGrouping = False,
      isMultiline = False,
      content = pure (from text)
    }

-- | Produce code with a symbol reference that is determined based on the imports and requirements for them.
importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Exp
importedSymbol moduleRef memberName =
  Exp
    { needsGrouping = False,
      isMultiline = False,
      content = Code.importedSymbol moduleRef memberName
    }

chainApplication :: Exp -> [Exp] -> Exp
chainApplication function params =
  if all (not . (.isMultiline)) params && not function.isMultiline
    then
      Exp
        { needsGrouping = True,
          isMultiline = False,
          content =
            toUngroupedCodeSplice function
              <> foldMap (mappend " " . toGroupedCodeSplice) params
        }
    else
      Exp
        { needsGrouping = True,
          isMultiline = False,
          content =
            toGroupedCodeSplice function
              <> fmap (Splice.indent 2) (foldMap (mappend "\n" . toGroupedCodeSplice) params)
        }

binApplication :: Exp -> Exp -> Exp
binApplication function param =
  if function.isMultiline && param.isMultiline
    then
      Exp
        { needsGrouping = True,
          isMultiline = True,
          content =
            function.content <> fmap (Splice.indent 2) ("\n" <> toGroupedCodeSplice param)
        }
    else
      Exp
        { needsGrouping = True,
          isMultiline = False,
          content =
            function.content <> " " <> toGroupedCodeSplice param
        }

decimalLiteral :: (Integral a) => a -> Exp
decimalLiteral =
  Exp False False . pure . to . UnilineSplice.decimal

stringLiteral :: Text -> Exp
stringLiteral text =
  Exp False isMultiline (pure splice)
  where
    isMultiline =
      case processedLines of
        [] -> False
        [_] -> False
        _ -> True
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
