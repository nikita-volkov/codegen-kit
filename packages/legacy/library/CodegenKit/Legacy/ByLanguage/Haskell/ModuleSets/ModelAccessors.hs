{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CodegenKit.Legacy.ByLanguage.Haskell.ModuleSets.ModelAccessors
  ( moduleName,
    modules,
  )
where

import Coalmine.MultilineTextBuilder qualified as B
import CodegenKit.Legacy.ByLanguage.Haskell.ModuleSets.ModelAccessors.Templates qualified as Templates
import CodegenKit.Legacy.ByLanguage.Haskell.ModuleSets.ModelTypes qualified as ModelTypesPackage
import CodegenKit.Legacy.ByLanguage.Haskell.Packaging qualified as Packaging
import CodegenKit.Legacy.ByLanguage.Haskell.PackagingPresets.Dependencies qualified as Dependencies
import CodegenKit.Legacy.ByLanguage.Haskell.Snippets qualified as Snippets
import CodegenKit.Legacy.Prelude hiding (product, sum)

-- * --

moduleName :: Name
moduleName =
  "accessors"

-- * --

modules ::
  [(Text, Text, [(Text, Text, Int, Int)])] ->
  [(Text, [(Text, Text)])] ->
  Packaging.Modules
modules hasFieldConfigs hasVariantConfigs =
  Packaging.v1Module True moduleName deps contentByNs
  where
    deps =
      [ Dependencies.basePrelude
      ]
    contentByNs ns =
      content
        "BasePrelude"
        (Snippets.moduleRef ns ModelTypesPackage.moduleName)
        (Snippets.moduleRef ns moduleName)
        hasFieldConfigs
        hasVariantConfigs

content ::
  B.Builder ->
  B.Builder ->
  B.Builder ->
  [(Text, Text, [(Text, Text, Int, Int)])] ->
  [(Text, [(Text, Text)])] ->
  Text
content preludeModuleRef typesModuleRef moduleRef hasFieldConfigs hasVariantConfigs =
  Templates.module_
    preludeModuleRef
    typesModuleRef
    moduleRef
    ( hasFieldConfigs
        & fmap
          ( \(lcFieldName, ucFieldName, instanceConfigs) ->
              hasFieldDecls lcFieldName ucFieldName instanceConfigs
          )
        & join
        & B.intercalate "\n\n"
    )
    ( hasVariantConfigs
        & fmap
          ( \(ucVariantName, instanceConfigs) ->
              hasVariantDecls ucVariantName instanceConfigs
          )
        & join
        & B.intercalate "\n\n"
    )

-- * --

hasFieldDecls lcFieldName ucFieldName instanceConfigs =
  Templates.hasFieldClass lcFieldName ucFieldName
    : instanceDecls
  where
    instanceDecls =
      instanceConfigs
        & fmap
          ( \(hostName, fieldSig, fieldIndex, fieldAmount) ->
              Templates.hasFieldInstance ucFieldName hostName fieldSig fieldIndex fieldAmount
          )

hasVariantDecls ucVariantName instanceConfigs =
  Templates.hasVariantClass ucVariantName
    : instanceDecls
  where
    instanceDecls =
      instanceConfigs
        & fmap
          ( \(ucSumName, variantSig) ->
              Templates.hasVariantInstance ucVariantName ucSumName variantSig
          )
