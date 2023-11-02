{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module CodegenKit.Legacy.ByLanguage.Haskell.Dsls.Package where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as Exp
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies (Dependencies)
import CodegenKit.Legacy.Versioning (VersionRange)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

toFileset :: Package -> Fileset
toFileset =
  error "TODO"

data Package

package :: Component -> Package
package =
  error "TODO"

-- | Package component. Library, executable, test.
data Component = Component
  { exposedModules :: Set Text,
    otherModules :: Set Text
  }

data Module = Module
  { path :: Path,
    name :: Text,
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
    name =
      Text.intercalate "." namespace
    extensions =
      code.extensions
    dependencies =
      code.dependencies
    content style =
      [i|
        module $name where

        $importsSplice

        $bodySplice
      |]
      where
        importsSplice =
          code.imports
            & Map.toAscList
            & fmap
              ( \(name, symbols) ->
                  case Map.lookup name aliasMap of
                    Nothing ->
                      Left (ImportsBlockTemplate.UnqualifiedImport name (Just (Set.toList symbols)))
                    Just alias ->
                      Right (ImportsBlockTemplate.QualifiedImport name alias)
              )
            & partitionEithers
            & ( \(unqualified, qualified) ->
                  ImportsBlockTemplate.ImportsBlock {..}
              )
            & CodeTemplate.compileCodeTemplate style
        aliasMap =
          Map.fromList aliasMapList
        bodySplice =
          code.splice style alias
          where
            alias qualified =
              case Map.lookup qualified aliasMap of
                Just alias -> alias
                Nothing -> qualified

data Code = Code
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules and symbols that are requested to be imported.
    imports :: Map Text (Set Text),
    splice :: CodeTemplate.CodeStyle -> (Text -> Text) -> Splice
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

importing ::
  -- | Module name.
  Text ->
  -- | Symbol name.
  Text ->
  (Text -> Splice) ->
  Code
importing moduleName symbolName symbolCont =
  Code
    { extensions = mempty,
      dependencies = mempty,
      imports = Map.singleton moduleName (Set.singleton symbolName),
      splice = \_ deref ->
        symbolCont case deref moduleName of
          "" -> symbolName
          prefix -> mconcat [prefix, ".", symbolName]
    }

expCode :: Exp -> Code
expCode exp =
  Code
    { extensions = exp.extensions,
      dependencies = exp.dependencies,
      imports = exp.imports,
      splice = \style -> Exp.ungroupedExp . exp.baseExp style
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
    baseExp :: CodeTemplate.CodeStyle -> (Text -> Text) -> Exp.Exp
  }

importedExp ::
  Maybe Dependency ->
  -- | Module name.
  Text ->
  -- | Symbol name.
  Text ->
  Exp
importedExp dependency moduleName symbolName =
  Exp
    { extensions = mempty,
      dependencies = foldMap (.dependencies) dependency,
      imports = Map.singleton moduleName (Set.singleton symbolName),
      baseExp = \_ deref -> Exp.reference (deref moduleName) symbolName
    }

data Dependency = Dependency
  { dependencies :: Dependencies
  }

dependency :: Text -> VersionRange -> Dependency
dependency packageName versionRange =
  error "TODO"
