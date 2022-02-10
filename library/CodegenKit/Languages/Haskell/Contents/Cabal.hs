module CodegenKit.Languages.Haskell.Contents.Cabal where

import Coalmine.Inter
import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import CodegenKit.Prelude hiding (Version)
import qualified TextBuilder as B'

-- *

cabal ::
  PackageName ->
  Text ->
  Version ->
  [ModuleRef] ->
  [ModuleRef] ->
  [Dependency] ->
  Builder
cabal packageName synopsis version exposedModuleList otherModuleList dependencyList =
  [i|
    cabal-version: 3.0

    name: $packageName
    synopsis: $synopsis
    version: $version
    build-type: Simple

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

-- *

newtype ModuleRef
  = ModuleRef Builder
  deriving (ToMultilineTextBuilder)

-- *

newtype Dependency
  = Dependency Builder
  deriving (ToMultilineTextBuilder)

rangeDependency :: PackageName -> Version -> Version -> Dependency
rangeDependency name min max =
  Dependency
    [i|
      $name >=$min && <$max
    |]

-- *

newtype PackageName
  = PackageName Builder
  deriving (ToMultilineTextBuilder)

-- *

newtype Version
  = Version Builder
  deriving (ToMultilineTextBuilder)

version2 :: Word -> Word -> Version
version2 a b =
  Version . B.uniline . mconcat $
    [ B'.unsignedDecimal a,
      ".",
      B'.unsignedDecimal b
    ]
