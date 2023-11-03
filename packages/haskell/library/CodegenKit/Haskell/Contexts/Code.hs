module CodegenKit.Haskell.Contexts.Code
  ( -- * Execution
    toPackageModule,
    toModuleFile,
    toModuleText,
    toHeadlessSplice,

    -- ** Execution configs
    ModuleConfig (..),
    Preferences (..),

    -- * Code
    Code,
    importing,
  )
where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Prelude
import CodegenKit.Haskell.Contexts.Package qualified as Package
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

-- * Execution

toPackageModule :: ModuleConfig -> Code -> Package.Module
toPackageModule moduleConfig code =
  Package.module_ compiler
  where
    compiler packagePreferences =
      toPackageCompiledModule moduleConfig (preferences packagePreferences) code

    preferences Package.Preferences {..} =
      Preferences {..}

toPackageCompiledModule :: ModuleConfig -> Preferences -> Code -> Package.CompiledModule
toPackageCompiledModule moduleConfig preferences code =
  Package.CompiledModule
    { path =
        Path.addExtension "hs" . foldMap (fromString . to) $ moduleConfig.namespace,
      name =
        moduleName,
      requestedExtensions =
        compiledCode.extensions,
      requestedDependencies =
        compiledCode.dependencies,
      content =
        content preferences compiledCode.imports compiledCode.splice
    }
  where
    moduleName =
      Text.intercalate "." moduleConfig.namespace

    compiledCode =
      code.compile preferences aliasModule

    aliasMap =
      Map.fromList moduleConfig.importAliases

    aliasModule qualified =
      case Map.lookup qualified aliasMap of
        Just alias -> alias
        Nothing -> qualified

    content preferences imports bodySplice =
      [i|
        module $moduleName where

        $importsSplice

        $bodySplice
      |]
      where
        importsSplice =
          imports
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
          where
            style =
              CodeTemplate.CodeStyle
                { importQualifiedPost = preferences.importQualifiedPost,
                  overloadedRecordDot = preferences.overloadedRecordDot,
                  strictData = preferences.strictData
                }

toModuleFile :: ModuleConfig -> Preferences -> Code -> Fileset
toModuleFile =
  error "TODO"

toModuleText :: ModuleConfig -> Preferences -> Code -> Text
toModuleText =
  error "TODO"

toHeadlessSplice :: CodeConfig -> Preferences -> Code -> Splice
toHeadlessSplice config preferences code =
  (code.compile preferences config.aliasModule).splice

toCompiledCode :: CodeConfig -> Preferences -> Code -> CompiledCode
toCompiledCode config preferences code =
  code.compile preferences config.aliasModule

-- ** Execution configs

data ModuleConfig = ModuleConfig
  { -- | Components of the namespace including the module name.
    namespace :: [Text],
    -- | Qualified import alias map.
    -- If a requested import is not present in it,
    -- it will be imported unqualified.
    importAliases :: [(Text, Text)]
  }

newtype CodeConfig = CodeConfig
  { -- | Function attempting to look up an alias or
    -- qualified reference for a module,
    -- producing empty text otherwise.
    aliasModule :: Text -> Text
  }

data Preferences = Preferences
  { strictData :: Bool,
    overloadedRecordDot :: Bool,
    importQualifiedPost :: Bool
  }

-- * Code

newtype Code = Code
  { compile ::
      Preferences ->
      (Text -> Text) ->
      CompiledCode
  }
  deriving (Semigroup, Monoid)

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

importing ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  -- | Code given a qualified member name.
  (Text -> Code) ->
  Code
importing moduleName memberName cont =
  Code \preferences aliasModule ->
    let ref =
          case aliasModule moduleName of
            "" -> memberName
            prefix -> mconcat [prefix, ".", memberName]
        CompiledCode {..} =
          (cont ref).compile preferences aliasModule
     in CompiledCode
          { extensions,
            dependencies,
            splice,
            imports =
              Map.alter
                ( \case
                    Nothing -> Just (Set.singleton ref)
                    Just set -> Just (Set.insert ref set)
                )
                moduleName
                imports
          }
