module CodegenKit.HaskellPackage.Aggregates.CodeGrouping where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies (Dependencies)
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

data CodeGrouping = CodeGrouping
  { needsGrouping :: Bool,
    isMultiline :: Bool
  }

instance Semigroup CodeGrouping where
  left <> right =
    CodeGrouping
      { needsGrouping = left.needsGrouping || right.needsGrouping,
        isMultiline = left.isMultiline || right.isMultiline
      }

instance Monoid CodeGrouping where
  mempty =
    CodeGrouping
      { needsGrouping = False,
        isMultiline = False
      }
