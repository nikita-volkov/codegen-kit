module CodegenKit.Languages.Haskell.Packaging
  ( -- *
    toFileSet,

    -- *
    PackageName,
    spinalPackageName,

    -- *
    Version,
    version2,
  )
where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Languages.Haskell.Contents.Cabal (PackageName, Version, spinalPackageName, version2)
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

toHiddenModuleSet :: Modules -> Set [Name]
toHiddenModuleSet (Modules files exposed hidden) =
  error "TODO"

-- | Render cabal file contents.
toCabalContents ::
  -- | Package name.
  PackageName ->
  -- | Synopsis.
  Text ->
  Version ->
  Modules ->
  Text
toCabalContents packageName synopsis version modules =
  CabalContents.contents
    packageName
    synopsis
    version
    exposed
    hidden
    dependencies
  where
    exposed = fmap CabalContents.nameListModuleRef . toList . toExposedModuleSet $ modules
    hidden = fmap CabalContents.nameListModuleRef . toList . toHiddenModuleSet $ modules
    dependencies =
      [ d "base" 4 [12] 5 [],
        d "bytestring" 0 [10] 0 [12],
        d "containers" 0 [6] 0 [7],
        d "scientific" 0 [3] 0 [4],
        d "text" 1 [2] 3 [],
        d "time" 1 [9] 2 [],
        d "uuid" 1 [3] 2 [],
        d "vector" 0 [12] 0 [13]
      ]
      where
        d name minHead minTail maxHead maxTail =
          CabalContents.rangeDependency
            (CabalContents.spinalPackageName name)
            (CabalContents.listVersion minHead minTail)
            (CabalContents.listVersion maxHead maxTail)

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
  -- | Is it exposed?
  Bool ->
  -- | Module reference.
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
