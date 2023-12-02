-- | General expression printer DSL managing metadata about dependencies, imports and extensions.
--
-- Serves as the basis for Type and Value expression DSLs.
module CodegenKit.HaskellPackage.Dsls.Exp where

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

toGroupedCodeSplice :: Exp -> Code.Context Splice
toGroupedCodeSplice x =
  if x.codeGrouping.needsGrouping
    then
      if x.codeGrouping.isMultiline
        then "( " <> fmap (Splice.indent 2) x.codeSplice <> "\n)"
        else "(" <> x.codeSplice <> ")"
    else x.codeSplice

toUngroupedCodeSplice :: Exp -> Code.Context Splice
toUngroupedCodeSplice = (.codeSplice)

data Exp = Exp
  { codeGrouping :: CodeGrouping.CodeGrouping,
    codeSplice :: Code.Context Splice
  }

chainApplication :: Exp -> [Exp] -> Exp
chainApplication function params =
  if all (not . (.codeGrouping.isMultiline)) params && not function.codeGrouping.isMultiline
    then
      Exp
        { codeGrouping =
            CodeGrouping.CodeGrouping
              { needsGrouping = True,
                isMultiline = False
              },
          codeSplice =
            toUngroupedCodeSplice function
              <> foldMap (mappend " " . toGroupedCodeSplice) params
        }
    else error "TODO"
