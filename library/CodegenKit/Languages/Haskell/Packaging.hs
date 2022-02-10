module CodegenKit.Languages.Haskell.Packaging
  ( -- *
    toFileSet,

    -- *
    Modules,
    inNamespace,
    module_,

    -- *
    CabalContents.Version,
    CabalContents.listVersion,
  )
where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Languages.Haskell.Contents.Cabal as CabalContents
import CodegenKit.Packaging (FileSet)
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
  fromList . toList $ exposed

toHiddenModuleSet :: Modules -> Set [Name]
toHiddenModuleSet (Modules files exposed hidden) =
  fromList . toList $ hidden

-- | Render cabal file contents.
toCabalContents ::
  -- | Package name.
  Name ->
  -- | Synopsis.
  Text ->
  CabalContents.Version ->
  Modules ->
  Text
toCabalContents packageName synopsis version modules =
  CabalContents.contents
    (CabalContents.spinalPackageName packageName)
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

toCabalFileSet :: Name -> Text -> CabalContents.Version -> Modules -> FileSet
toCabalFileSet packageName synopsis version modules =
  Packaging.fromFile filePath contents
  where
    filePath =
      fromString . toString
        . flip mappend ".cabal"
        . Name.toSpinalCaseTextBuilder
        $ packageName
    contents =
      toCabalContents packageName synopsis version modules

toModulesFileSet :: DirPath -> Modules -> FileSet
toModulesFileSet srcDirPath (Modules files _ _) =
  foldMap file files
  where
    file (filePath, render) =
      Packaging.fromFile
        (Paths.inDir srcDirPath filePath)
        (render [])

-- |
-- Generate all package files including @.cabal@.
toFileSet ::
  -- | Package name.
  Name ->
  -- | Synopsis.
  Text ->
  CabalContents.Version ->
  Modules ->
  FileSet
toFileSet packageName synopsis version modules =
  case inNamespace [packageName] modules of
    modules ->
      toCabalFileSet packageName synopsis version modules
        <> toModulesFileSet "library" modules

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
  -- | Module name.
  Name ->
  -- | Module contents rendering function from compiled namespace.
  ([Name] -> Text) ->
  Modules
module_ exposed name contents =
  Modules
    (pure (filePath, contents))
    (if exposed then pure [name] else empty)
    (if exposed then empty else pure [name])
  where
    filePath =
      fromString . toString . flip mappend ".hs" . Name.toUpperCamelCaseText $ name

-- * Helpers

moduleDirPath :: [Name] -> DirPath
moduleDirPath =
  foldMap (fromString . toString . Name.toUpperCamelCaseText)
