{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module CodegenKit.ByLanguage.Haskell.Dsls.Package where

import Coalmine.Prelude
import CodegenKit.ByLanguage.Haskell.Composers.Exp qualified as Exp
import CodegenKit.Dependencies (Dependencies)
import CodegenKit.Versioning (VersionRange)
import Data.Set qualified as Set

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
    dependencies :: Dependencies,
    content :: Text
  }

data Import

data Code = Code
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules that are requested to be imported.
    imports :: Set Text,
    splice :: (Text -> Text) -> Splice
  }

instance Semigroup Code where
  left <> right =
    Code
      { extensions = left.extensions <> right.extensions,
        dependencies = left.dependencies <> right.dependencies,
        imports = left.imports <> right.imports,
        splice = left.splice <> right.splice
      }

instance Monoid Code where
  mempty =
    Code
      { extensions = mempty,
        dependencies = mempty,
        imports = mempty,
        splice = mempty
      }

expCode :: Exp -> Code
expCode exp =
  Code
    { extensions = exp.extensions,
      dependencies = exp.dependencies,
      imports = exp.imports,
      splice = Exp.ungroupedExp . exp.baseExp
    }

-- | For explicitly adding dependencies to code.
dependenciesCode :: Dependencies -> Code
dependenciesCode dependencies =
  Code
    { extensions = mempty,
      dependencies,
      imports = mempty,
      splice = mempty
    }

data Exp = Exp
  { extensions :: Set Text,
    dependencies :: Dependencies,
    -- | Modules that are requested to be imported.
    imports :: Set Text,
    baseExp :: (Text -> Text) -> Exp.Exp
  }

refExp ::
  Maybe Dependency ->
  -- | Module name.
  Text ->
  -- | Symbol name.
  Text ->
  Exp
refExp dependency moduleName symbolName =
  Exp
    { extensions = mempty,
      dependencies = foldMap (.dependencies) dependency,
      imports = Set.singleton moduleName,
      baseExp = \deref -> Exp.reference (deref moduleName) symbolName
    }

data Dependency = Dependency
  { dependencies :: Dependencies
  }

dependency :: Text -> VersionRange -> Dependency
dependency packageName versionRange =
  error "TODO"
