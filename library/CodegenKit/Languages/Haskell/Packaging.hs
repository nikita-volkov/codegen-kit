module CodegenKit.Languages.Haskell.Packaging where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Prelude

-- *

-- |
-- A collection of modules with their requirements for
-- the package configuration files.
-- E.g., being exposed or hidden.
newtype HaskellPackage
  = HaskellPackage
      (Acc (FilePath, Text))
  deriving (Semigroup, Monoid)

-- |
-- Prepend a directory path to all contents of this package.
inDir :: DirPath -> HaskellPackage -> HaskellPackage
inDir path (HaskellPackage contents) =
  HaskellPackage $ fmap (first (Paths.inDir path)) contents
