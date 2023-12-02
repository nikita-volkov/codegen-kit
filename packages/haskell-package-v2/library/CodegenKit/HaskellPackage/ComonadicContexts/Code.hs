module CodegenKit.HaskellPackage.ComonadicContexts.Code where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import TextBuilderDev qualified as UnilineSplice
import "comonad" Control.Comonad
import "comonad" Control.Comonad.Env
import "comonad" Control.Comonad.Store
import "comonad" Control.Comonad.Traced

newtype Config = Config
  { -- | Function attempting to look up an alias or
    -- qualified reference for a module,
    -- producing empty text otherwise.
    aliasNamespace :: Text -> Text
  }

instance Semigroup Config where
  left <> right =
    Config
      { aliasNamespace = \input ->
          let leftResult = left.aliasNamespace input
           in if Text.null leftResult
                then right.aliasNamespace input
                else leftResult
      }

instance Monoid Config where
  mempty =
    Config
      { aliasNamespace = const Text.empty
      }

-- |
-- The most useful bit of this type is the Applicative instance, which lets you specify how you want the renderings to compose, while having the metadata composed automatically.
newtype Context a = Context
  { envt :: EnvT CodeRequirements ((->) Config) a
  }

deriving instance Functor Context

deriving instance Applicative Context

instance Comonad Context where
  extract = extract . (.envt)
  duplicate (Context envt) = unsafeCoerce (duplicate envt)

deriving instance ComonadApply Context

deriving instance ComonadEnv CodeRequirements Context

deriving instance ComonadTraced Config Context

instance (Semigroup a) => Semigroup (Context a) where
  Context (EnvT leftCodeRequirements leftPrinter) <> Context (EnvT rightCodeRequirements rightPrinter) =
    Context
      ( EnvT
          (leftCodeRequirements <> rightCodeRequirements)
          (leftPrinter <> rightPrinter)
      )

instance (Monoid a) => Monoid (Context a) where
  mempty =
    Context (EnvT mempty mempty)

instance (IsString a) => IsString (Context a) where
  fromString x =
    Context (EnvT mempty (const (fromString x)))

run :: Context a -> (CodeRequirements, Config -> a)
run = runEnvT . (.envt)

context :: CodeRequirements -> (Config -> a) -> Context a
context codeRequirements compile =
  Context (EnvT codeRequirements compile)

dependency :: (Monoid a) => Text -> Word -> [Word] -> Word -> [Word] -> Context a
dependency packageName minHead minTail maxHead maxTail =
  context
    (CodeRequirements.fromDependency packageName minHead minTail maxHead maxTail)
    mempty

importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Context Splice
importedSymbol namespace name =
  context requirements compiler
  where
    requirements =
      CodeRequirements.fromSymbolImport namespace name
    compiler config =
      case config.aliasNamespace namespace of
        "" -> to name
        alias -> mconcat [to alias, ".", to name]

importedModule :: Text -> Context Text
importedModule namespace =
  context
    (CodeRequirements.fromModuleImport namespace)
    (\config -> config.aliasNamespace namespace)
