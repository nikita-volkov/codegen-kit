module CodegenKit.HaskellPackage.ComonadicContexts.ValueExp where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeGrouping qualified as CodeGrouping
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.ComonadicContexts.Code qualified as Code
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

newtype Context a = Context
  { envt :: EnvT CodeGrouping.CodeGrouping Code.Context a
  }
  deriving (Functor, Applicative, ComonadApply, ComonadEnv CodeGrouping.CodeGrouping, ComonadTraced Code.Config)

instance Comonad Context where
  extract = extract . (.envt)
  duplicate (Context envt) = unsafeCoerce (duplicate envt)

toCode :: Context a -> Code.Context a
toCode = lower . (.envt)
