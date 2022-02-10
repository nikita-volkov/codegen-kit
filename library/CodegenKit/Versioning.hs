module CodegenKit.Versioning
  ( -- *
  )
where

import CodegenKit.Prelude
import qualified TextBuilder

data VersionBounds
  = VersionBounds
      !Word
      -- ^ Min bound version part 1.
      !Word
      -- ^ Min bound version part 2.
      !Word
      -- ^ Max bound version part 1.
      !Word
      -- ^ Max bound version part 2.

instance Semigroup VersionBounds where
  VersionBounds lMin1 lMin2 lMax1 lMax2 <> VersionBounds rMin1 rMin2 rMax1 rMax2 =
    error "TODO"

-- *

data Version
  = Version !Word ![Word]
  deriving (Eq)

instance Ord Version where
  compare l r =
    compare (versionParts l) (versionParts r)

instance ToTextBuilder Version where
  toTextBuilder (Version h t) =
    TextBuilder.unsignedDecimal h
      <> foldMap (mappend "." . TextBuilder.unsignedDecimal) t

instance ToMultilineTextBuilder Version where
  toMultilineTextBuilder =
    toMultilineTextBuilder . toTextBuilder

instance ToText Version where
  toText =
    toText . toTextBuilder

instance ToString Version where
  toString =
    toString . toTextBuilder

versionParts :: Version -> [Word]
versionParts (Version head tail) =
  head : tail
