module CodegenKit.Legacy.Dependencies
  ( -- * --
    Dependencies,

    -- ** --
    toList,

    -- ** --
    singleton,
    add,
  )
where

import CodegenKit.Legacy.Prelude hiding (singleton, toList)
import CodegenKit.Legacy.Versioning (VersionRange, pattern VersionRange)
import CodegenKit.Legacy.Versioning qualified as Versioning
import Data.Map.Strict qualified as Map

-- * --

newtype Dependencies
  = Dependencies (Map Text VersionRange)
  deriving (Show, Eq)

instance Semigroup Dependencies where
  Dependencies lMap <> Dependencies rMap =
    Dependencies
      $ Map.unionWith (<>) lMap rMap

instance Monoid Dependencies where
  mempty =
    Dependencies mempty

-- ** --

toList :: Dependencies -> [(Text, VersionRange)]
toList =
  Map.toAscList . coerce

-- ** --

singleton :: Text -> Word -> [Word] -> Word -> [Word] -> Dependencies
singleton packageName minHead minTail maxHead maxTail =
  singletonVersionRange
    packageName
    ( VersionRange
        (Just (Versioning.Version minHead minTail))
        (Just (Versioning.Version maxHead maxTail))
    )

singletonVersionRange :: Text -> VersionRange -> Dependencies
singletonVersionRange packageName versionRange =
  Dependencies (Map.singleton packageName versionRange)

add :: Text -> Word -> [Word] -> Word -> [Word] -> Dependencies -> Dependencies
add packageName minHead minTail maxHead maxTail =
  addVersionRange
    packageName
    ( VersionRange
        (Just (Versioning.Version minHead minTail))
        (Just (Versioning.Version maxHead maxTail))
    )

addVersionRange :: Text -> VersionRange -> Dependencies -> Dependencies
addVersionRange packageName versionRange =
  mapMap . flip Map.alter packageName $ \case
    Nothing -> Just versionRange
    Just existingVersionRange -> Just (existingVersionRange <> versionRange)

-- * Helpers

mapMap :: (Map Text VersionRange -> Map Text VersionRange) -> Dependencies -> Dependencies
mapMap = coerce
