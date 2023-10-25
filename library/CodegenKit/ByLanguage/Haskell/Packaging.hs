module CodegenKit.ByLanguage.Haskell.Packaging
  ( -- * --
    toFileset,

    -- * --
    Modules,
    inNamespace,
    module_,
    v1Module,
    v2Module,

    -- * --
    Dependency,
    dependency,

    -- * --
    CabalContents.Version,
    CabalContents.listVersion,

    -- * --
    PackageLocation,
    hackagePackageLocation,
    githubPackageLocation,
  )
where

import qualified Coalmine.Fileset as Fileset
import qualified Coalmine.Name as Name
import qualified CodegenKit.ByLanguage.Haskell.Composers.Cabal as CabalContents
import qualified CodegenKit.ByLanguage.Haskell.Composers.Stack as StackFileSet
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude
import qualified CodegenKit.Versioning as Versioning
import qualified Data.Map.Strict as Map

-- * --

-- |
-- A collection of modules with their requirements for
-- the package configuration files.
-- E.g., being exposed or hidden.
data Modules
  = Modules
      -- | List of files.
      !(Acc (Path, [Name] -> Text))
      -- | Exposed modules.
      !(Acc [Name])
      -- | Hidden modules.
      !(Acc [Name])
      -- | Package dependencies.
      !(Map Text Versioning.VersionRange)
      -- | Stack dependencies.
      ![StackFileSet.ExtraDep]

instance Semigroup Modules where
  Modules l1 l2 l3 l4 l5 <> Modules r1 r2 r3 r4 r5 =
    Modules (l1 <> r1) (l2 <> r2) (l3 <> r3) (Map.unionWith (<>) l4 r4) (l5 <> r5)

instance Monoid Modules where
  mempty = Modules mempty mempty mempty mempty mempty

-- ** --

toExposedModuleSet :: Modules -> Set [Name]
toExposedModuleSet (Modules _ exposed _ _ _) =
  fromList . toList $ exposed

toHiddenModuleSet :: Modules -> Set [Name]
toHiddenModuleSet (Modules _ _ hidden _ _) =
  fromList . toList $ hidden

toDependencyList :: Modules -> [(Text, Versioning.VersionRange)]
toDependencyList (Modules _ _ _ deps _) =
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
               Versioning.VersionRange
                 (Just (Versioning.Version minHead minTail))
                 (Just (Versioning.Version maxHead maxTail))
               ) ->
            CabalContents.rangeDependency
              (CabalContents.plainPackageName name)
              (CabalContents.listVersion minHead minTail)
              (CabalContents.listVersion maxHead maxTail)

toCabalFileSet :: Name -> Text -> CabalContents.Version -> Modules -> Fileset
toCabalFileSet packageName synopsis version modules =
  Fileset.file filePath contents
  where
    filePath =
      fromString
        . to
        . flip mappend ".cabal"
        . Name.toSpinalCaseTextBuilder
        $ packageName
    contents =
      toCabalContents packageName synopsis version modules

toModulesFileSet :: Path -> Modules -> Fileset
toModulesFileSet srcDirPath (Modules files _ _ _ _) =
  foldMap file files
  where
    file (filePath, render) =
      Fileset.file
        (srcDirPath <> filePath)
        (render [])

toStackExtraDeps :: Modules -> [StackFileSet.ExtraDep]
toStackExtraDeps (Modules _ _ _ _ x) =
  x

-- |
-- Generate all package files including @.cabal@.
toFileset ::
  -- | Package name.
  Name ->
  -- | Synopsis.
  Text ->
  -- | Package version.
  CabalContents.Version ->
  -- | Stack resolver. If we want to produce a stack file that is.
  Maybe Text ->
  Modules ->
  Fileset
toFileset packageName synopsis version stackResolver modules =
  mconcat
    [ toCabalFileSet packageName synopsis version modules,
      toModulesFileSet "library" modules,
      foldMap (\stackResolver -> StackFileSet.fileSet stackResolver (toStackExtraDeps modules)) stackResolver
    ]

-- ** --

inNamespace :: [Name] -> Modules -> Modules
inNamespace ns (Modules files exposed hidden dependencies stackExtraDeps) =
  Modules
    files'
    (fmap (mappend ns) exposed)
    (fmap (mappend ns) hidden)
    dependencies
    stackExtraDeps
  where
    files' =
      files & fmap (bimap prependPath prependModuleName)
      where
        prependPath =
          mappend (moduleDirPath ns)
        prependModuleName render =
          render . flip mappend ns

{-# DEPRECATED module_ "Use v1Module" #-}
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
module_ = v1Module

v1Module ::
  -- | Is it exposed?
  Bool ->
  -- | Module name.
  Name ->
  -- | Package dependencies.
  [Dependency] ->
  -- | Module contents rendering function from compiled namespace.
  ([Name] -> Text) ->
  Modules
v1Module exposed name dependencies contents =
  Modules
    (pure (filePath, contents))
    (if exposed then pure [name] else empty)
    (if exposed then empty else pure [name])
    (Map.fromListWith (<>) . fmap dependencyTuple $ dependencies)
    (foldMap (\(Dependency _ _ x) -> x) dependencies)
  where
    filePath =
      fromString . to . flip mappend ".hs" . Name.toUpperCamelCaseText $ name

v2Module ::
  -- | Is it exposed?
  Bool ->
  -- | Module name.
  Name ->
  -- | Package dependencies.
  [Dependency] ->
  -- | Module contents rendering function from qualified module name.
  (Text -> Text) ->
  Modules
v2Module exposed name dependencies contents =
  Modules
    (pure (filePath, adaptedContents))
    (if exposed then pure [name] else empty)
    (if exposed then empty else pure [name])
    (Map.fromListWith (<>) . fmap dependencyTuple $ dependencies)
    (foldMap (\(Dependency _ _ x) -> x) dependencies)
  where
    adaptedContents namespace =
      contents $ to @Text $ Snippets.moduleRef namespace name

    filePath =
      fromString . to . flip mappend ".hs" . Name.toUpperCamelCaseText $ name

-- * --

data Dependency
  = Dependency
      -- | Package name.
      !Text
      -- | Package version bounds.
      !Versioning.VersionRange
      -- | Extra dependencies for stack.
      ![StackFileSet.ExtraDep]

-- ** --

dependencyTuple :: Dependency -> (Text, Versioning.VersionRange)
dependencyTuple (Dependency a b _) =
  (a, b)

-- ** --

dependency :: Text -> Word -> [Word] -> Word -> [Word] -> [PackageLocation] -> Dependency
dependency packageName minHead minTail maxHead maxTail stackExtraDeps =
  Dependency
    packageName
    ( Versioning.VersionRange
        (Just (Versioning.Version minHead minTail))
        (Just (Versioning.Version maxHead maxTail))
    )
    (fmap (.stackExtraDep) stackExtraDeps)

-- * --

-- |
-- How to acquire the source code of the package.
data PackageLocation = PackageLocation
  { stackExtraDep :: ~StackFileSet.ExtraDep
  }

hackagePackageLocation :: Text -> Word -> [Word] -> PackageLocation
hackagePackageLocation name versionHead versionTail =
  PackageLocation
    { stackExtraDep =
        StackFileSet.hackageExtraDep name versionHead versionTail
    }

githubPackageLocation :: Text -> Text -> Text -> Text -> PackageLocation
githubPackageLocation name group repo commitHash =
  PackageLocation
    { stackExtraDep =
        StackFileSet.githubExtraDep name group repo commitHash
    }

-- * Helpers

moduleDirPath :: [Name] -> Path
moduleDirPath =
  foldMap (fromString . to . Name.toUpperCamelCaseText)
