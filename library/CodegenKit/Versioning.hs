module CodegenKit.Versioning
  ( -- * --
    VersionBounds (..),

    -- * --
    Version (..),
  )
where

import CodegenKit.Prelude
import qualified Data.Attoparsec.Text as Attoparsec
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

data Version = Version
  { versionHead :: !Word,
    versionTail :: ![Word]
  }
  deriving (Eq, Show, Generic)

instance Ord Version where
  compare l r =
    compare (versionParts l) (versionParts r)

instance CompactPrinting Version where
  toCompactBuilder (Version h t) =
    toCompactBuilder h
      <> foldMap (mappend "." . toCompactBuilder) t

instance BroadPrinting Version where
  toBroadBuilder = to . toCompactBuilder

instance ToJSON Version where
  toJSON = toJSON . printCompactAs @Text

instance ToJSONKey Version where
  toJSONKey = printCompactAs @Text >$< toJSONKey

instance LenientParser Version where
  lenientParser = do
    head <- Attoparsec.decimal
    tail <- many tailSegmentParser
    return (Version head tail)
    where
      tailSegmentParser = do
        Attoparsec.char '.'
        Attoparsec.decimal

versionParts :: Version -> [Word]
versionParts (Version head tail) =
  head : tail
