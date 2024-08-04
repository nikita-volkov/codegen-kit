module CodegenKit.HaskellPackage.Contexts.CompiledCode
  ( CompiledCode (..),
    fromSplice,
    fromSymbolImport,
    fromModuleImport,
    fromExport,
    fromExtension,
    fromDependency,
    mapSplice,
    addSymbolImport,
    addModuleImport,
    addExport,
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
    symbolImports :: Map Text (Set Text),
    moduleImports :: Set Text,
    exports :: [Text],
    splice :: Splice
  }

instance Semigroup CompiledCode where
  left <> right =
    CompiledCode
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        symbolImports = Map.unionWith Set.union left.symbolImports right.symbolImports,
        moduleImports = Set.union left.moduleImports right.moduleImports,
        exports = left.exports <> right.exports,
        splice = left.splice <> right.splice
      }

instance Monoid CompiledCode where
  mempty =
    CompiledCode
      { extensions = mempty,
        dependencies = mempty,
        symbolImports = mempty,
        moduleImports = mempty,
        exports = mempty,
        splice = mempty
      }

instance IsString CompiledCode where
  fromString string =
    CompiledCode
      { extensions = mempty,
        dependencies = mempty,
        symbolImports = mempty,
        moduleImports = mempty,
        exports = mempty,
        splice = fromString string
      }

fromSplice :: Splice -> CompiledCode
fromSplice splice =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = mempty,
      exports = mempty,
      splice
    }

fromSymbolImport :: Text -> Text -> CompiledCode
fromSymbolImport moduleName symbolName =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = Map.singleton moduleName (Set.singleton symbolName),
      moduleImports = mempty,
      exports = mempty,
      splice = mempty
    }

fromModuleImport :: Text -> CompiledCode
fromModuleImport moduleName =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = Set.singleton moduleName,
      exports = mempty,
      splice = mempty
    }

fromExport :: Text -> CompiledCode
fromExport export =
  addExport export mempty

fromExtension :: Text -> CompiledCode
fromExtension extension =
  CompiledCode
    { extensions = Set.singleton extension,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = mempty,
      exports = mempty,
      splice = mempty
    }

fromDependency :: Text -> Word -> [Word] -> Word -> [Word] -> CompiledCode
fromDependency packageName minHead minTail maxHead maxTail =
  CompiledCode
    { extensions = mempty,
      dependencies = Dependencies.singleton packageName minHead minTail maxHead maxTail,
      symbolImports = mempty,
      moduleImports = mempty,
      exports = mempty,
      splice = mempty
    }

mapSplice :: (Splice -> Splice) -> CompiledCode -> CompiledCode
mapSplice mapper compiledCode =
  compiledCode
    { splice = mapper compiledCode.splice
    }

mapSymbolImports :: (Map Text (Set Text) -> Map Text (Set Text)) -> CompiledCode -> CompiledCode
mapSymbolImports mapper compiledCode =
  compiledCode
    { symbolImports = mapper compiledCode.symbolImports
    }

mapModuleImports :: (Set Text -> Set Text) -> CompiledCode -> CompiledCode
mapModuleImports mapper compiledCode =
  compiledCode
    { moduleImports = mapper compiledCode.moduleImports
    }

addSymbolImport :: Text -> Text -> CompiledCode -> CompiledCode
addSymbolImport moduleName symbolName =
  mapSymbolImports
    $ Map.alter
      ( \case
          Nothing -> Just (Set.singleton symbolName)
          Just set -> Just (Set.insert symbolName set)
      )
      moduleName

addModuleImport :: Text -> CompiledCode -> CompiledCode
addModuleImport moduleName =
  mapModuleImports
    $ Set.insert moduleName

addExport :: Text -> CompiledCode -> CompiledCode
addExport export code =
  code {exports = export : code.exports}
