{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module CodegenKit.ByLanguage.Haskell.Dsls.Package
  (
  )
where

import Coalmine.Prelude hiding (Dependencies)
import CodegenKit.Dependencies (Dependencies)
import CodegenKit.Dependencies qualified as Dependencies
import CodegenKit.Versioning (VersionRange)
import CodegenKit.Versioning qualified as VersionRange

toFileset :: Package -> Fileset
toFileset =
  error "TODO"

data Package

package :: Component -> Package
package =
  error "TODO"

-- | Package component. Library, executable, test.
data Component

data Module = Module
  { extensions :: Set Text,
    ghc :: VersionRange,
    dependencies :: Dependencies,
    content :: Text
  }

data Import

data Code = Code
  { ghc :: VersionRange,
    extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules that are requested to be imported.
    imports :: Set Text,
    splice :: (Text -> Text) -> Splice
  }

exp :: Exp -> Code
exp =
  error "TODO"

data Exp = Exp
