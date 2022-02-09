module CodegenKit.Languages.Haskell.Packaging where

import qualified Coalmine.Name as Name
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
