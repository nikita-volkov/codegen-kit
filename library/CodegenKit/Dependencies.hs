module CodegenKit.Dependencies
  ( -- * --
    Dependencies,

    -- ** --
    toList,

    -- ** --
    singleton,
  )
where

import CodegenKit.Prelude hiding (singleton, toList)
import qualified CodegenKit.Versioning as Versioning
import qualified Data.Map.Strict as Map

-- * --

newtype Dependencies
  = Dependencies (Map Text Versioning.VersionRange)

instance Semigroup Dependencies where
  Dependencies lMap <> Dependencies rMap =
    Dependencies
      $ Map.unionWith (<>) lMap rMap

instance Monoid Dependencies where
  mempty =
    Dependencies mempty

-- ** --

toList :: Dependencies -> [(Text, Versioning.VersionRange)]
toList =
  Map.toAscList . coerce

-- ** --

singleton :: Text -> Word -> [Word] -> Word -> [Word] -> Dependencies
singleton packageName minHead minTail maxHead maxTail =
  Dependencies
    $ Map.singleton
      packageName
      ( Versioning.VersionRange
          (Just (Versioning.Version minHead minTail))
          (Just (Versioning.Version maxHead maxTail))
      )
