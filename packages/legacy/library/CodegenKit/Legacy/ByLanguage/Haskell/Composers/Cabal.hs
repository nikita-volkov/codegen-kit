module CodegenKit.Legacy.ByLanguage.Haskell.Composers.Cabal
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
    plainModuleRef,

    -- * --
    Dependency,
    rangeDependency,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import Coalmine.MultilineTextBuilder qualified as B
import Coalmine.Name qualified as Name
import CodegenKit.Legacy.Prelude
import TextBuilderDev qualified as B'

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

    name: ${packageName}
    synopsis: ${synopsis}
    version: ${version}

    common base
      default-language: Haskell2010
      default-extensions:
        ApplicativeDo
        BangPatterns
        BinaryLiterals
        BlockArguments
        ConstraintKinds
        DataKinds
        DefaultSignatures
        DeriveAnyClass
        DeriveDataTypeable
        DeriveFoldable
        DeriveFunctor
        DeriveGeneric
        DeriveTraversable
        DerivingVia
        DuplicateRecordFields
        EmptyCase
        EmptyDataDecls
        FlexibleContexts
        FlexibleInstances
        FunctionalDependencies
        GADTs
        GeneralizedNewtypeDeriving
        HexFloatLiterals
        ImportQualifiedPost
        LambdaCase
        LiberalTypeSynonyms
        MultiParamTypeClasses
        MultiWayIf
        NamedFieldPuns
        NoFieldSelectors
        NoImplicitPrelude
        NoMonomorphismRestriction
        NumericUnderscores
        OverloadedLabels
        OverloadedRecordDot
        OverloadedStrings
        ParallelListComp
        PatternGuards
        PatternSynonyms
        QuasiQuotes
        RankNTypes
        RecordWildCards
        ScopedTypeVariables
        StandaloneDeriving
        StrictData
        TemplateHaskell
        TupleSections
        TypeApplications
        TypeFamilies
        TypeOperators
        UndecidableInstances
        ViewPatterns

    library
      import: base
      hs-source-dirs: library
      exposed-modules:
        ${exposedModules}
      other-modules:
        ${otherModules}
      build-depends:
        ${dependencies}
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
  deriving (BroadPrinting)

plainPackageName :: Text -> PackageName
plainPackageName =
  PackageName . B.uniline . from @Text

spinalPackageName :: Name -> PackageName
spinalPackageName =
  PackageName . B.uniline . Name.toSpinalCaseTextBuilder

-- * --

newtype Version
  = Version Builder
  deriving (BroadPrinting, IsString)

listVersion :: Word -> [Word] -> Version
listVersion head tail =
  Version
    . B.uniline
    . B'.intercalate "."
    . fmap B'.unsignedDecimal
    $ head
    : tail

version2 :: Word -> Word -> Version
version2 a b =
  Version
    . B.uniline
    $ if b == 0
      then B'.unsignedDecimal a
      else
        mconcat
          $ [ B'.unsignedDecimal a,
              ".",
              B'.unsignedDecimal b
            ]

-- * --

newtype ModuleRef
  = ModuleRef Builder
  deriving (BroadPrinting)

nameListModuleRef :: [Name] -> ModuleRef
nameListModuleRef =
  ModuleRef . B.uniline . B'.intercalate "." . fmap Name.toUpperCamelCaseTextBuilder

plainModuleRef :: Text -> ModuleRef
plainModuleRef =
  ModuleRef . to

-- * --

newtype Dependency
  = Dependency Builder
  deriving (BroadPrinting)

rangeDependency :: PackageName -> Version -> Version -> Dependency
rangeDependency name min max =
  Dependency
    [i|
      ${name} >=${min} && <${max}
    |]
