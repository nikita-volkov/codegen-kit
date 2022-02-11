module CodegenKit.LanguageUtils.Haskell.Packaging
  ( -- *
    toFileSet,

    -- *
    Modules,
    inNamespace,
    module_,

    -- *
    Dependency,
    dependency,

    -- *
    CabalContents.Version,
    CabalContents.listVersion,
  )
where

import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.LanguageUtils.Haskell.Contents.Cabal as CabalContents
import qualified CodegenKit.LanguageUtils.Haskell.FileSets.Stack as StackFileSet
import CodegenKit.Packaging (FileSet)
import qualified CodegenKit.Packaging as Packaging
import CodegenKit.Prelude
import qualified CodegenKit.Versioning as Versioning
import qualified Data.Map.Strict as Map

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
      !(Map Text Versioning.VersionBounds)
      -- ^ Package dependencies.

instance Semigroup Modules where
  Modules l1 l2 l3 l4 <> Modules r1 r2 r3 r4 =
    Modules (l1 <> r1) (l2 <> r2) (l3 <> r3) (Map.unionWith (<>) l4 r4)

instance Monoid Modules where
  mempty = Modules mempty mempty mempty mempty

-- **

toExposedModuleSet :: Modules -> Set [Name]
toExposedModuleSet (Modules _ exposed _ _) =
  fromList . toList $ exposed

toHiddenModuleSet :: Modules -> Set [Name]
toHiddenModuleSet (Modules _ _ hidden _) =
  fromList . toList $ hidden

toDependencyList :: Modules -> [(Text, Versioning.VersionBounds)]
toDependencyList (Modules _ _ _ deps) =
  deps & Map.toAscList

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
      toDependencyList modules
        <&> \( name,
               Versioning.VersionBounds
                 (Versioning.Version minHead minTail)
                 (Versioning.Version maxHead maxTail)
               ) ->
            CabalContents.rangeDependency
              (CabalContents.plainPackageName name)
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
toModulesFileSet srcDirPath (Modules files _ _ _) =
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
      mconcat
        [ StackFileSet.fileSet,
          toCabalFileSet packageName synopsis version modules,
          toModulesFileSet "library" modules
        ]

-- **

inNamespace :: [Name] -> Modules -> Modules
inNamespace ns (Modules files exposed hidden dependencies) =
  Modules
    files'
    (fmap (mappend ns) exposed)
    (fmap (mappend ns) hidden)
    dependencies
  where
    files' =
      files & fmap (bimap prependPath prependModuleName)
      where
        prependPath =
          Paths.inDir (moduleDirPath ns)
        prependModuleName render =
          render . flip mappend ns

module_ ::
  -- | Is it exposed?
  Bool ->
  -- | Module name.
  Name ->
  -- | Package dependencies.
  [Dependency] ->
  -- | Module contents rendering function from compiled namespace.
  ([Name] -> Text) ->
  Modules
module_ exposed name dependencies contents =
  Modules
    (pure (filePath, contents))
    (if exposed then pure [name] else empty)
    (if exposed then empty else pure [name])
    (Map.fromListWith (<>) . fmap dependencyTuple $ dependencies)
  where
    filePath =
      fromString . toString . flip mappend ".hs" . Name.toUpperCamelCaseText $ name

-- *

data Dependency
  = Dependency
      !Text
      !Versioning.VersionBounds

-- **

dependencyTuple :: Dependency -> (Text, Versioning.VersionBounds)
dependencyTuple (Dependency a b) =
  (a, b)

-- **

dependency :: Text -> Word -> [Word] -> Word -> [Word] -> Dependency
dependency packageName minHead minTail maxHead maxTail =
  Dependency
    packageName
    ( Versioning.VersionBounds
        (Versioning.Version minHead minTail)
        (Versioning.Version maxHead maxTail)
    )

-- * Helpers

moduleDirPath :: [Name] -> DirPath
moduleDirPath =
  foldMap (fromString . toString . Name.toUpperCamelCaseText)
