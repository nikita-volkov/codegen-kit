module CodegenKit.HaskellPackage.Contexts2.Code2 where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.Contexts.Package qualified as Package
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
    aliasModule :: Text -> Text
  }

instance Semigroup Config where
  left <> right =
    Config
      { aliasModule = \input ->
          let leftResult = left.aliasModule input
           in if Text.null leftResult
                then right.aliasModule input
                else leftResult
      }

instance Monoid Config where
  mempty =
    Config
      { aliasModule = const Text.empty
      }

newtype Context a = Context
  { envt :: EnvT CodeRequirements ((->) Config) a
  }

deriving instance Functor Context

deriving instance Applicative Context

instance Comonad Context where
  extract = extract . (.envt)
  duplicate (Context envt) = unsafeCoerce (duplicate envt)

deriving instance ComonadEnv CodeRequirements Context

deriving instance ComonadTraced Config Context

context :: CodeRequirements -> (Config -> a) -> Context a
context codeRequirements compile =
  Context (EnvT codeRequirements compile)
