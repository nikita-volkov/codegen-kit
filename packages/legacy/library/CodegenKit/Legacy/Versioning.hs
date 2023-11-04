module CodegenKit.Legacy.Versioning
  ( -- * --
    VersionRange,
    pattern VersionRange,

    -- * --
    Version (..),
  )
where

import CodegenKit.Legacy.Prelude
import Data.Attoparsec.Text qualified as Attoparsec
import StructureKit.OpenRange qualified as OpenRange

-- * --

type VersionRange =
  OpenRange.OpenRange Version

pattern VersionRange :: Maybe Version -> Maybe Version -> VersionRange
pattern VersionRange from upto = OpenRange.OpenRange from upto

{-# COMPLETE VersionRange #-}

-- * --

data Version = Version
  { versionHead :: Word,
    versionTail :: [Word]
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
