module CodegenKit.HaskellPackage.Aggregates.CodeRequirements
  ( CodeRequirements (..),
    fromSplice,
    fromSymbolImport,
    fromModuleImport,
    fromExtension,
    fromDependency,
    addSymbolImport,
    addModuleImport,
    addDependency,
  )
where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Package qualified as Package
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies (Dependencies)
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

-- | Compiled code with metadata.
data CodeRequirements = CodeRequirements
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules and symbols that are requested to be imported.
    symbolImports :: Map Text (Set Text),
    moduleImports :: Set Text
  }

instance Semigroup CodeRequirements where
  left <> right =
    CodeRequirements
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        symbolImports = Map.unionWith Set.union left.symbolImports right.symbolImports,
        moduleImports = Set.union left.moduleImports right.moduleImports
      }

instance Monoid CodeRequirements where
  mempty =
    CodeRequirements
      { extensions = mempty,
        dependencies = mempty,
        symbolImports = mempty,
        moduleImports = mempty
      }

fromSplice :: Splice -> CodeRequirements
fromSplice splice =
  CodeRequirements
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = mempty
    }

fromSymbolImport :: Text -> Text -> CodeRequirements
fromSymbolImport moduleName symbolName =
  CodeRequirements
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = Map.singleton moduleName (Set.singleton symbolName),
      moduleImports = mempty
    }

fromModuleImport :: Text -> CodeRequirements
fromModuleImport moduleName =
  CodeRequirements
    { extensions = mempty,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = Set.singleton moduleName
    }

fromExtension :: Text -> CodeRequirements
fromExtension extension =
  CodeRequirements
    { extensions = Set.singleton extension,
      dependencies = mempty,
      symbolImports = mempty,
      moduleImports = mempty
    }

fromDependency :: Text -> Word -> [Word] -> Word -> [Word] -> CodeRequirements
fromDependency packageName minHead minTail maxHead maxTail =
  CodeRequirements
    { extensions = mempty,
      dependencies = Dependencies.singleton packageName minHead minTail maxHead maxTail,
      symbolImports = mempty,
      moduleImports = mempty
    }

mapSymbolImports :: (Map Text (Set Text) -> Map Text (Set Text)) -> CodeRequirements -> CodeRequirements
mapSymbolImports mapper requirements =
  requirements
    { symbolImports = mapper requirements.symbolImports
    }

mapModuleImports :: (Set Text -> Set Text) -> CodeRequirements -> CodeRequirements
mapModuleImports mapper requirements =
  requirements
    { moduleImports = mapper requirements.moduleImports
    }

mapDependencies :: (Dependencies -> Dependencies) -> CodeRequirements -> CodeRequirements
mapDependencies mapper requirements =
  requirements
    { dependencies = mapper requirements.dependencies
    }

addSymbolImport :: Text -> Text -> CodeRequirements -> CodeRequirements
addSymbolImport moduleName symbolName =
  mapSymbolImports
    $ Map.alter
      ( \case
          Nothing -> Just (Set.singleton symbolName)
          Just set -> Just (Set.insert symbolName set)
      )
      moduleName

addModuleImport :: Text -> CodeRequirements -> CodeRequirements
addModuleImport moduleName =
  mapModuleImports
    $ Set.insert moduleName

addDependency :: Text -> Word -> [Word] -> Word -> [Word] -> CodeRequirements -> CodeRequirements
addDependency packageName minHead minTail maxHead maxTail =
  mapDependencies
    $ Dependencies.add packageName minHead minTail maxHead maxTail
