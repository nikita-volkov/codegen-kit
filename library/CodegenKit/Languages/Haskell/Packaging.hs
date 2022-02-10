module CodegenKit.Languages.Haskell.Packaging where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Languages.Haskell.Contents.Cabal as CabalContents
import qualified CodegenKit.Packaging as Packaging
import CodegenKit.Prelude

-- *

-- |
-- A collection of modules with their requirements for
-- the package configuration files.
-- E.g., being exposed or hidden.
data Modules
  = Modules
      !(Acc (FilePath, [Name] -> Text))
      -- ^ List of files.
      !(Acc [Name])
      -- ^ Exposed modules.
      !(Acc [Name])
      -- ^ Hidden modules.

instance Semigroup Modules where
  Modules l1 l2 l3 <> Modules r1 r2 r3 =
    Modules (l1 <> r1) (l2 <> r2) (l3 <> r3)

instance Monoid Modules where
  mempty = Modules mempty mempty mempty

-- **

toExposedModuleSet :: Modules -> Set [Name]
toExposedModuleSet (Modules files exposed hidden) =
  error "TODO"

-- | Render cabal file contents.
toCabalContents ::
  -- | Package name.
  Name ->
  Modules ->
  Text
toCabalContents packageName (Modules _ exposed hidden) =
  error "TODO"

-- |
-- Generate all package files including @.cabal@.
toFileSet ::
  -- | Package name.
  Name ->
  Modules ->
  Packaging.FileSet
toFileSet =
  error "TODO"

-- **

inNamespace :: [Name] -> Modules -> Modules
inNamespace ns (Modules files exposed hidden) =
  Modules
    files'
    (fmap (mappend ns) exposed)
    (fmap (mappend ns) hidden)
  where
    files' =
      files & fmap (bimap prependPath prependModuleName)
      where
        prependPath =
          Paths.inDir (moduleDirPath ns)
        prependModuleName render =
          render . mappend ns

module_ ::
  -- | Module namespace.
  [Name] ->
  -- | Module contents rendering function from compiled namespace.
  ([Name] -> Text) ->
  Modules
module_ =
  error "TODO"

-- * Helpers

moduleDirPath :: [Name] -> DirPath
moduleDirPath =
  foldMap (fromString . toString . Name.toUpperCamelCaseText)
