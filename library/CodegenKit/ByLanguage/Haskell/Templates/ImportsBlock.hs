module CodegenKit.ByLanguage.Haskell.Templates.ImportsBlock where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.ByLanguage.Haskell.CodeTemplate
import Data.Text qualified as Text

data ImportsBlock = ImportsBlock
  { unqualified :: [UnqualifiedImport],
    qualified :: [QualifiedImport]
  }

instance CodeTemplate ImportsBlock where
  compileCodeTemplate style ImportsBlock {..} =
    TextBlock.intercalate "\n" compiledImportList
    where
      compiledImportList =
        compiledUnqualifiedImportList <> compiledQualifiedImportList
      compiledQualifiedImportList =
        qualified
          & nubSort
          & fmap (compileCodeTemplate style)
      compiledUnqualifiedImportList =
        unqualified
          & nubSort
          & fmap (compileCodeTemplate style)

data UnqualifiedImport = UnqualifiedImport
  { qualifiedName :: Text,
    symbols :: [Text]
  }
  deriving (Ord, Eq)

instance CodeTemplate UnqualifiedImport where
  compileCodeTemplate _ UnqualifiedImport {..} =
    [j|import $qualifiedName ($symbolsSplice)|]
    where
      symbolsSplice =
        symbols
          & Text.intercalate ", "

data QualifiedImport = QualifiedImport
  { qualifiedName :: Text,
    -- | If empty, it's considered empty.
    alias :: Text
  }
  deriving (Ord, Eq)

instance CodeTemplate QualifiedImport where
  compileCodeTemplate style QualifiedImport {..} =
    if Text.null alias
      then
        if style.importQualifiedPost
          then [j|import $qualifiedName qualified|]
          else [j|import qualified $qualifiedName|]
      else
        if style.importQualifiedPost
          then [j|import $qualifiedName qualified as $alias|]
          else [j|import qualified $qualifiedName as $alias|]
