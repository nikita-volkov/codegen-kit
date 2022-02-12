module CodegenKit.DependencyManagement
  ( -- *
    Dependencies,

    -- **
    toList,

    -- **
    singleton,
  )
where

import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Prelude hiding (singleton, toList)
import qualified CodegenKit.Versioning as Versioning
import qualified Data.Map.Strict as Map

-- *

newtype Dependencies
  = Dependencies (Map Text Versioning.VersionBounds)

instance Semigroup Dependencies where
  Dependencies lMap <> Dependencies rMap =
    Dependencies $
      Map.unionWith (<>) lMap rMap

instance Monoid Dependencies where
  mempty =
    Dependencies mempty

-- **

toList :: Dependencies -> [(Text, Versioning.VersionBounds)]
toList =
  Map.toAscList . coerce

-- **

singleton :: Text -> Word -> [Word] -> Word -> [Word] -> Dependencies
singleton packageName minHead minTail maxHead maxTail =
  Dependencies $
    Map.singleton
      packageName
      ( Versioning.VersionBounds
          (Versioning.Version minHead minTail)
          (Versioning.Version maxHead maxTail)
      )
