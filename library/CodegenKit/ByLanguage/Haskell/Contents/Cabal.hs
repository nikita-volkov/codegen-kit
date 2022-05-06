module CodegenKit.ByLanguage.Haskell.Contents.Cabal
  ( -- * --
    contents,

    -- * --
    PackageName,
    plainPackageName,
    spinalPackageName,

    -- * --
    Version,
    listVersion,
    version2,

    -- * --
    ModuleRef,
    nameListModuleRef,

    -- * --
    Dependency,
    rangeDependency,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import CodegenKit.Prelude hiding (Version)
import qualified TextBuilderDev as B'

-- * --

contents ::
  PackageName ->
  Text ->
  Version ->
  [ModuleRef] ->
  [ModuleRef] ->
  [Dependency] ->
  Text
contents packageName synopsis version exposedModuleList otherModuleList dependencyList =
  [i|
    cabal-version: 3.0

    name: $packageName
    synopsis: $synopsis
    version: $version

    library
      hs-source-dirs: library
      default-extensions: ApplicativeDo, BangPatterns, BinaryLiterals, BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingVia, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, HexFloatLiterals, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, NumericUnderscores, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeApplications, TypeFamilies, TypeOperators, ViewPatterns
      default-language: Haskell2010
      exposed-modules:
        $exposedModules
      other-modules:
        $otherModules
      build-depends:
        $dependencies
  |]
  where
    exposedModules =
      B.intercalate "\n" . coerce $ exposedModuleList
    otherModules =
      B.intercalate "\n" . coerce $ otherModuleList
    dependencies =
      B.intercalate ",\n" . coerce $ dependencyList

-- * --

newtype PackageName
  = PackageName Builder
  deriving (PrettyPrinting)

plainPackageName :: Text -> PackageName
plainPackageName =
  PackageName . B.uniline . fromText

spinalPackageName :: Name -> PackageName
spinalPackageName =
  PackageName . B.uniline . Name.toSpinalCaseTextBuilder

-- * --

newtype Version
  = Version Builder
  deriving (PrettyPrinting)

listVersion :: Word -> [Word] -> Version
listVersion head tail =
  Version . B.uniline . B'.intercalate "."
    . fmap B'.unsignedDecimal
    $ head : tail

version2 :: Word -> Word -> Version
version2 a b =
  Version . B.uniline $
    if b == 0
      then B'.unsignedDecimal a
      else
        mconcat $
          [ B'.unsignedDecimal a,
            ".",
            B'.unsignedDecimal b
          ]

-- * --

newtype ModuleRef
  = ModuleRef Builder
  deriving (PrettyPrinting)

nameListModuleRef :: [Name] -> ModuleRef
nameListModuleRef =
  ModuleRef . B.uniline . B'.intercalate "." . fmap Name.toUpperCamelCaseTextBuilder

-- * --

newtype Dependency
  = Dependency Builder
  deriving (PrettyPrinting)

rangeDependency :: PackageName -> Version -> Version -> Dependency
rangeDependency name min max =
  Dependency
    [i|
      $name >=$min && <$max
    |]
