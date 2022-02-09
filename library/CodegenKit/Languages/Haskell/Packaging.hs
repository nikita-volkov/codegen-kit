module CodegenKit.Languages.Haskell.Packaging where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Packaging as Packaging
import CodegenKit.Prelude

-- *

-- |
-- A collection of modules with their requirements for
-- the package configuration files.
-- E.g., being exposed or hidden.
data HaskellPackage
  = HaskellPackage
      !(Acc (FilePath, [Name], Text))
      !(Acc (FilePath, [Name], Text))

instance Semigroup HaskellPackage where
  HaskellPackage a b <> HaskellPackage c d =
    HaskellPackage (a <> c) (b <> d)

instance Monoid HaskellPackage where
  mempty = HaskellPackage mempty mempty

instance ToString HaskellPackage where
  toString = toString . toMultilineTextBuilder

instance ToText HaskellPackage where
  toText = toText . toMultilineTextBuilder

instance ToTextBuilder HaskellPackage where
  toTextBuilder = toTextBuilder . toMultilineTextBuilder

instance ToMultilineTextBuilder HaskellPackage where
  toMultilineTextBuilder = toMultilineTextBuilder . toFileSet

-- **

-- |
-- Convert to separate vectors of exposed and hidden modules
toFileVecs :: HaskellPackage -> (BVec (FilePath, Text), BVec (FilePath, Text))
toFileVecs =
  error "TODO"

-- | Render cabal file contents.
toCabalContents ::
  -- | Package name.
  Text ->
  HaskellPackage ->
  Text
toCabalContents =
  error "TODO"

-- |
-- Generate all package files including .cabal.
toFileSet :: HaskellPackage -> Packaging.FileSet
toFileSet =
  error "TODO"

-- **

inNamespace :: [Name] -> HaskellPackage -> HaskellPackage
inNamespace ns (HaskellPackage exposedModules hiddenModules) =
  HaskellPackage
    (error "TODO")
    (error "TODO")
