module CodegenKit.Legacy.ByLanguage.Haskell.Packaging
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
    Cabal.Version,
    Cabal.listVersion,

    -- * --
    PackageLocation,
    hackagePackageLocation,
    githubPackageLocation,
  )
where

import Coalmine.Fileset qualified as Fileset
import Coalmine.Name qualified as Name
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Cabal qualified as Cabal
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Stack qualified as Stack
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import CodegenKit.Legacy.Prelude
import CodegenKit.Legacy.Versioning qualified as Versioning
import Data.Map.Strict qualified as Map

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
      ![Stack.ExtraDep]

instance Semigroup Modules where
  Modules l1 l2 l3 l4 l5 <> Modules r1 r2 r3 r4 r5 =
    Modules (l1 <> r1) (l2 <> r2) (l3 <> r3) (Map.unionWith (<>) l4 r4) (l5 <> r5)

instance Monoid Modules where
  mempty = Modules mempty mempty mempty mempty mempty

-- ** --

toExposedModuleSet :: Modules -> Set [Name]
toExposedModuleSet (Modules _ exposed _ _ _) =
  fromList . sort . toList $ exposed

toHiddenModuleSet :: Modules -> Set [Name]
toHiddenModuleSet (Modules _ _ hidden _ _) =
  fromList . sort . toList $ hidden

toDependencyList :: Modules -> [(Text, Versioning.VersionRange)]
toDependencyList (Modules _ _ _ deps _) =
  deps & Map.toAscList

-- | Render cabal file contents.
toCabalContents ::
  -- | Package name.
  Name ->
  -- | Synopsis.
  Text ->
  Cabal.Version ->
  Modules ->
  Text
toCabalContents packageName synopsis version modules =
  Cabal.contents
    (Cabal.spinalPackageName packageName)
    synopsis
    version
    exposed
    hidden
    dependencies
  where
    exposed = fmap Cabal.nameListModuleRef . toList . toExposedModuleSet $ modules
    hidden = fmap Cabal.nameListModuleRef . toList . toHiddenModuleSet $ modules
    dependencies =
      toDependencyList modules
        <&> \(name, versionRange) -> case versionRange of
          Versioning.VersionRange
            (Just (Versioning.Version minHead minTail))
            (Just (Versioning.Version maxHead maxTail)) ->
              Cabal.rangeDependency
                (Cabal.plainPackageName name)
                (Cabal.listVersion minHead minTail)
                (Cabal.listVersion maxHead maxTail)
          _ -> error "TODO"

toCabalFileSet :: Name -> Text -> Cabal.Version -> Modules -> Fileset
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

toStackExtraDeps :: Modules -> [Stack.ExtraDep]
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
  Cabal.Version ->
  -- | Stack resolver. If we want to produce a stack file that is.
  Maybe Text ->
  Modules ->
  Fileset
toFileset packageName synopsis version stackResolver modules =
  mconcat
    [ toCabalFileSet packageName synopsis version modules,
      toModulesFileSet "library" modules,
      foldMap (\stackResolver -> Stack.fileSet stackResolver (toStackExtraDeps modules)) stackResolver
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
      ![Stack.ExtraDep]

-- ** --

dependencyTuple :: Dependency -> (Text, Versioning.VersionRange)
dependencyTuple (Dependency a b _) =
  (a, b)

-- ** --

dependency :: Text -> Word -> [Word] -> Word -> [Word] -> [PackageLocation] -> Dependency
dependency packageName minHead minTail maxHead maxTail packageLocations =
  Dependency
    packageName
    ( Versioning.VersionRange
        (Just (Versioning.Version minHead minTail))
        (Just (Versioning.Version maxHead maxTail))
    )
    (fmap (.stackExtraDep) packageLocations)

-- * --

-- |
-- How to acquire the source code of the package.
data PackageLocation = PackageLocation
  { stackExtraDep :: ~Stack.ExtraDep
  }

hackagePackageLocation :: Text -> Word -> [Word] -> PackageLocation
hackagePackageLocation name versionHead versionTail =
  PackageLocation
    { stackExtraDep =
        Stack.hackageExtraDep name versionHead versionTail
    }

githubPackageLocation :: Text -> Text -> Text -> Text -> PackageLocation
githubPackageLocation name group repo commitHash =
  PackageLocation
    { stackExtraDep =
        Stack.githubExtraDep name group repo commitHash
    }

-- * Helpers

moduleDirPath :: [Name] -> Path
moduleDirPath =
  foldMap (fromString . to . Name.toUpperCamelCaseText)
