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
      !(Acc (FilePath, Text))
      -- ^ List of files.
      !(Acc [Name])
      -- ^ Exposed modules.
      !(Acc [Name])
      -- ^ Hidden modules.

instance Semigroup HaskellPackage where
  HaskellPackage l1 l2 l3 <> HaskellPackage r1 r2 r3 =
    HaskellPackage (l1 <> r1) (l2 <> r2) (l3 <> r3)

instance Monoid HaskellPackage where
  mempty = HaskellPackage mempty mempty mempty

instance ToString HaskellPackage where
  toString = toString . toMultilineTextBuilder

instance ToText HaskellPackage where
  toText = toText . toMultilineTextBuilder

instance ToTextBuilder HaskellPackage where
  toTextBuilder = toTextBuilder . toMultilineTextBuilder

instance ToMultilineTextBuilder HaskellPackage where
  toMultilineTextBuilder = toMultilineTextBuilder . toFileSet

-- **

toExposedModuleSet :: HaskellPackage -> Set [Name]
toExposedModuleSet (HaskellPackage files exposed hidden) =
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
-- Generate all package files including @.cabal@.
toFileSet :: HaskellPackage -> Packaging.FileSet
toFileSet =
  error "TODO"

-- **

inNamespace :: [Name] -> HaskellPackage -> HaskellPackage
inNamespace ns (HaskellPackage files exposed hidden) =
  HaskellPackage
    (error "TODO")
    (error "TODO")
    (error "TODO")

module_ ::
  -- | Module namespace.
  [Name] ->
  -- | Module contents rendering function from compiled namespace.
  ([Name] -> Text) ->
  HaskellPackage
module_ =
  error "TODO"
