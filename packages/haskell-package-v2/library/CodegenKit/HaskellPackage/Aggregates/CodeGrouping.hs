module CodegenKit.HaskellPackage.Aggregates.CodeGrouping where

import Coalmine.Prelude

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
