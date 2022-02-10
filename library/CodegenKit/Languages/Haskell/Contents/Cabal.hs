module CodegenKit.Languages.Haskell.Contents.Cabal where

import Coalmine.Inter
import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.Languages.Haskell.Contents.Cabal.Layout as Layout
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (Version, product, sum)
import qualified TextBuilder as B'

-- *

cabal ::
  PackageName ->
  Text ->
  Version ->
  [ModuleRef] ->
  [ModuleRef] ->
  [Dependency] ->
  Builder
cabal packageName synopsis version exposedModules hiddenModules dependencies =
  Layout.cabal
    packageName
    synopsis
    version
    (Layout.moduleList . coerce $ exposedModules)
    (Layout.moduleList . coerce $ hiddenModules)
    (Layout.dependencyList . coerce $ dependencies)

-- *

newtype ModuleRef
  = ModuleRef Builder
  deriving (ToMultilineTextBuilder)

-- *

newtype Dependency
  = Dependency Builder
  deriving (ToMultilineTextBuilder)

rangeDependency :: PackageName -> Version -> Version -> Dependency
rangeDependency =
  error "TODO"

-- *

newtype PackageName
  = PackageName Builder
  deriving (ToMultilineTextBuilder)

-- *

newtype Version
  = Version Builder
  deriving (ToMultilineTextBuilder)

version2 :: Word -> Word -> Version
version2 a b =
  Version $
    Layout.version2
      (B'.unsignedDecimal a)
      (B'.unsignedDecimal b)
