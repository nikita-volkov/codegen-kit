module CodegenKit.ByLanguage.Haskell.Templates.ImportsBlock
  ( importsBlock,
  )
where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude hiding (Dependencies)
import CodegenKit.ByLanguage.Haskell.Composers.Exp qualified as Exp
import CodegenKit.Dependencies (Dependencies)
import CodegenKit.Dependencies qualified as Dependencies
import CodegenKit.Versioning (VersionRange)
import CodegenKit.Versioning qualified as VersionRange
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

importsBlock :: [Text] -> [(Text, Text)] -> TextBlock.Builder
importsBlock unqualifiedImports qualifiedImports =
  TextBlock.intercalate "\n" compiledImportList
  where
    compiledImportList =
      compiledUnqualifiedImportList <> compiledQualifiedImportList
    compiledQualifiedImportList =
      qualifiedImports
        & nubSort
        & fmap compileImport
      where
        compileImport (qualifiedName, alias) =
          if Text.null alias
            then [j|import qualified $qualifiedName|]
            else [j|import qualified $qualifiedName as $alias|]
    compiledUnqualifiedImportList =
      unqualifiedImports
        & nubSort
        & fmap compileImport
      where
        compileImport imported =
          [j|import $imported|]
