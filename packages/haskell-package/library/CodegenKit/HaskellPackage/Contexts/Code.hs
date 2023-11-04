module CodegenKit.HaskellPackage.Contexts.Code
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
    splice,
    importing,
    importedSymbol,
    indent,
    prefix,
    exp,
  )
where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Contexts.CompiledCode qualified as CompiledCode
import CodegenKit.HaskellPackage.Contexts.Exp qualified as Exp
import CodegenKit.HaskellPackage.Contexts.Package qualified as Package
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
toModuleFile moduleConfig preferences code =
  toPackageCompiledModule moduleConfig preferences code
    & \compiledModule ->
      Fileset.file compiledModule.path compiledModule.content

toModuleText :: ModuleConfig -> Preferences -> Code -> Text
toModuleText moduleConfig preferences code =
  toPackageCompiledModule moduleConfig preferences code
    & \compiledModule ->
      compiledModule.content

toHeadlessSplice :: CodeConfig -> Preferences -> Code -> Splice
toHeadlessSplice config preferences code =
  (code.compile preferences config.aliasModule).splice

toCompiledCode :: CodeConfig -> Preferences -> Code -> CompiledCode.CompiledCode
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
      CompiledCode.CompiledCode
  }
  deriving (Semigroup, Monoid)

instance IsString Code where
  fromString = Code . const . const . fromString

mapSplice :: (Splice -> Splice) -> Code -> Code
mapSplice mapper code =
  Code
    { compile = \preferences deref ->
        code.compile preferences deref
          & CompiledCode.mapSplice mapper
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
     in (cont ref).compile preferences aliasModule
          & CompiledCode.addImport moduleName memberName

-- | Produce code with a symbol reference that is determined based on the imports and produces requirements for them.
importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Code
importedSymbol moduleRef memberName =
  importing moduleRef memberName $ splice . to

splice :: Splice -> Code
splice splice =
  Code \_ _ ->
    CompiledCode.fromSplice splice

indent :: Int -> Code -> Code
indent spaces =
  mapSplice (Splice.indent spaces)

prefix :: Text -> Code -> Code
prefix prefix =
  mapSplice (Splice.prefixEachLine (to prefix))

exp :: Exp.Exp -> Code
exp exp =
  Code compile
  where
    compile preferences deref =
      Exp.toCompiledCode config exp
      where
        config =
          Exp.Config
            { overloadedRecordDot = preferences.overloadedRecordDot,
              deref
            }
