module CodegenKit.Versioning
  ( -- *
    VersionBounds (..),

    -- *
    Version (..),
  )
where

import qualified Coalmine.Range as Range
import CodegenKit.Prelude
import qualified TextBuilder

data VersionBounds
  = VersionBounds
      !Version
      -- ^ Min bound.
      !Version
      -- ^ Max bound.

instance Semigroup VersionBounds where
  VersionBounds lMin lMax <> VersionBounds rMin rMax =
    case Range.Range lMin lMax <> Range.Range rMin rMax of
      Range.Range min max ->
        VersionBounds min max

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
