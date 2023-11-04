module CodegenKit.HaskellPackage.Contexts.CompiledCode
  ( CompiledCode (..),
    fromSplice,
    fromImport,
    mapSplice,
    addImport,
  )
where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Package qualified as Package
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

-- * CompiledCode

-- | Compiled code with metadata.
data CompiledCode = CompiledCode
  { extensions :: Set Text,
    dependencies :: Dependencies.Dependencies,
    -- | Modules and symbols that are requested to be imported.
    imports :: Map Text (Set Text),
    splice :: Splice
  }

instance Semigroup CompiledCode where
  left <> right =
    CompiledCode
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        imports = Map.unionWith Set.union left.imports right.imports,
        splice = left.splice <> right.splice
      }

instance Monoid CompiledCode where
  mempty =
    CompiledCode
      { extensions = mempty,
        dependencies = mempty,
        imports = mempty,
        splice = mempty
      }

instance IsString CompiledCode where
  fromString string =
    CompiledCode
      { extensions = mempty,
        dependencies = mempty,
        imports = mempty,
        splice = fromString string
      }

fromSplice :: Splice -> CompiledCode
fromSplice splice =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      imports = mempty,
      splice
    }

fromImport :: Text -> Text -> CompiledCode
fromImport moduleName symbolName =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      imports = Map.singleton moduleName (Set.singleton symbolName),
      splice = mempty
    }

mapSplice :: (Splice -> Splice) -> CompiledCode -> CompiledCode
mapSplice mapper compiledCode =
  compiledCode
    { splice = mapper compiledCode.splice
    }

mapImports :: (Map Text (Set Text) -> Map Text (Set Text)) -> CompiledCode -> CompiledCode
mapImports mapper compiledCode =
  compiledCode
    { imports = mapper compiledCode.imports
    }

addImport :: Text -> Text -> CompiledCode -> CompiledCode
addImport moduleName symbolName =
  mapImports
    $ Map.alter
      ( \case
          Nothing -> Just (Set.singleton symbolName)
          Just set -> Just (Set.insert symbolName set)
      )
      moduleName
