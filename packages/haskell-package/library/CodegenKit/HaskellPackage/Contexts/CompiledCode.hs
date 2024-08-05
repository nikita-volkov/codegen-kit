module CodegenKit.HaskellPackage.Contexts.CompiledCode
  ( CompiledCode (..),
    fromSplice,
    fromSymbolImport,
    fromQualifiedModuleImport,
    fromUnqualifiedModuleImport,
    fromExport,
    fromExtension,
    fromDependency,
    mapSplice,
    addSymbolImport,
    addQualifiedModuleImport,
    addUnqualifiedModuleImport,
    addExport,
  )
where

import Coalmine.Prelude
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- * CompiledCode

-- | Compiled code with metadata.
data CompiledCode = CompiledCode
  { extensions :: Set Text,
    dependencies :: Dependencies.Dependencies,
    -- | Modules and symbols that are requested to be imported.
    symbolImports :: Map Text (Set Text),
    -- | Modules and whether they should be imported qualified.
    moduleImports :: Map Text Bool,
    exports :: [Text],
    splice :: Splice
  }

instance Semigroup CompiledCode where
  left <> right =
    CompiledCode
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        symbolImports = Map.unionWith Set.union left.symbolImports right.symbolImports,
        moduleImports = Map.unionWith (||) left.moduleImports right.moduleImports,
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

fromQualifiedModuleImport :: Text -> CompiledCode
fromQualifiedModuleImport moduleName =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = Map.singleton moduleName True,
      exports = mempty,
      splice = mempty
    }

fromUnqualifiedModuleImport :: Text -> CompiledCode
fromUnqualifiedModuleImport moduleName =
  CompiledCode
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = Map.singleton moduleName False,
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

addSymbolImport :: Text -> Text -> CompiledCode -> CompiledCode
addSymbolImport moduleName symbolName =
  mapSymbolImports
    $ Map.alter
      ( \case
          Nothing -> Just (Set.singleton symbolName)
          Just set -> Just (Set.insert symbolName set)
      )
      moduleName

addQualifiedModuleImport :: Text -> CompiledCode -> CompiledCode
addQualifiedModuleImport moduleName x =
  x
    { moduleImports =
        Map.insert moduleName True x.moduleImports
    }

addUnqualifiedModuleImport :: Text -> CompiledCode -> CompiledCode
addUnqualifiedModuleImport moduleName x =
  x
    { moduleImports =
        Map.insertWith (||) moduleName False x.moduleImports
    }

addExport :: Text -> CompiledCode -> CompiledCode
addExport export code =
  code {exports = export : code.exports}
