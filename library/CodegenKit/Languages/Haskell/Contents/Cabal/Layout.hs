module CodegenKit.Languages.Haskell.Contents.Cabal.Layout where

import Coalmine.Inter
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude hiding (intercalate, product, sum)

cabal packageName synopsis version exposedModules otherModules dependencies =
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

rangeDependency name min max =
  [i|
    $name >=$min && <$max
  |]

-- | 2-component version.
version2 a b =
  [i|
    $a.$b
  |]

moduleList =
  intercalate "\n"

dependencyList =
  intercalate ",\n"
