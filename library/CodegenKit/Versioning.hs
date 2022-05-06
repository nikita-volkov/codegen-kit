module CodegenKit.Versioning
  ( -- * --
    VersionBounds (..),

    -- * --
    Version (..),
  )
where

import CodegenKit.Prelude
import qualified StructureKit.Range as Range

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

-- * --

data Version
  = Version !Word ![Word]
  deriving (Eq, Show)

instance Ord Version where
  compare l r =
    compare (versionParts l) (versionParts r)

instance CompactPrinting Version where
  toCompactBuilder (Version h t) =
    toCompactBuilder h
      <> foldMap (mappend "." . toCompactBuilder) t

instance PrettyPrinting Version where
  toPrettyBuilder =
    toMultilineTextBuilder . toCompactBuilder

versionParts :: Version -> [Word]
versionParts (Version head tail) =
  head : tail
