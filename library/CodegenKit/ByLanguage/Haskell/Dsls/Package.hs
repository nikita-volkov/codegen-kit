{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module CodegenKit.ByLanguage.Haskell.Dsls.Package where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude
import CodegenKit.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.ByLanguage.Haskell.Composers.Exp qualified as Exp
import CodegenKit.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Dependencies (Dependencies)
import CodegenKit.Versioning (VersionRange)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

toFileset :: Package -> Fileset
toFileset =
  error "TODO"

data Package

package :: Component -> Package
package =
  error "TODO"

-- | Package component. Library, executable, test.
data Component

data Module = Module
  { path :: Path,
    extensions :: Set Text,
    dependencies :: Dependencies,
    content :: CodeTemplate.CodeStyle -> Text
  }

module_ ::
  -- | Namespace.
  [Text] ->
  -- | Qualified import alias map.
  -- If a requested import is not present in it,
  -- it will be imported unqualified.
  [(Text, Text)] ->
  Code ->
  Module
module_ namespace aliasMapList code =
  Module {..}
  where
    path =
      Path.addExtension "hs" . foldMap (fromString . to) $ namespace
    extensions =
      code.extensions
    dependencies =
      code.dependencies
    content style =
      to @Text
        $ importsSplice
        <> error "TODO: add body and head"
      where
        importsSplice =
          code.imports
            & Map.toAscList
            & fmap
              ( \(name, symbols) ->
                  case Map.lookup name aliasMap of
                    Nothing ->
                      Left (ImportsBlockTemplate.UnqualifiedImport name (Set.toList symbols))
                    Just alias ->
                      Right (ImportsBlockTemplate.QualifiedImport name alias)
              )
            & partitionEithers
            & ( \(unqualified, qualified) ->
                  ImportsBlockTemplate.ImportsBlock {..}
              )
            & CodeTemplate.compileCodeTemplate style
          where
            aliasMap =
              Map.fromList aliasMapList

data Code = Code
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules and symbols that are requested to be imported.
    imports :: Map Text (Set Text),
    splice :: (Text -> Text) -> Splice
  }

instance Semigroup Code where
  left <> right =
    Code
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        imports = Map.unionWith Set.union left.imports right.imports,
        splice = left.splice <> right.splice
      }

instance Monoid Code where
  mempty =
    Code
      { extensions = mempty,
        dependencies = mempty,
        imports = mempty,
        splice = mempty
      }

expCode :: Exp -> Code
expCode exp =
  Code
    { extensions = exp.extensions,
      dependencies = exp.dependencies,
      imports = exp.imports,
      splice = Exp.ungroupedExp . exp.baseExp
    }

-- | For explicitly adding dependencies to code.
dependenciesCode :: Dependencies -> Code
dependenciesCode dependencies =
  Code
    { extensions = mempty,
      dependencies,
      imports = mempty,
      splice = mempty
    }

data Exp = Exp
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules that are requested to be imported.
    imports :: Map Text (Set Text),
    baseExp :: (Text -> Text) -> Exp.Exp
  }

refExp ::
  Maybe Dependency ->
  -- | Module name.
  Text ->
  -- | Symbol name.
  Text ->
  Exp
refExp dependency moduleName symbolName =
  Exp
    { extensions = mempty,
      dependencies = foldMap (.dependencies) dependency,
      imports = Map.singleton moduleName (Set.singleton symbolName),
      baseExp = \deref -> Exp.reference (deref moduleName) symbolName
    }

data Dependency = Dependency
  { dependencies :: Dependencies
  }

dependency :: Text -> VersionRange -> Dependency
dependency packageName versionRange =
  error "TODO"
