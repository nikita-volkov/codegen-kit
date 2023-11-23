-- | Value expression printer DSL managing metadata about dependencies, imports and extensions.
module CodegenKit.HaskellPackage.Dsls.ValueExp where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeGrouping qualified as CodeGrouping
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.ComonadicContexts.Code qualified as Code
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as LegacyExp
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import TextBuilderDev qualified as UnilineSplice

toGroupedCodeSplice :: ValueExp -> Code.Context Splice
toGroupedCodeSplice =
  error "TODO"

toUngroupedCodeSplice :: ValueExp -> Code.Context Splice
toUngroupedCodeSplice =
  error "TODO"

data ValueExp = ValueExp
  { codeGrouping :: CodeGrouping.CodeGrouping,
    codeSplice :: Code.Context Splice
  }
