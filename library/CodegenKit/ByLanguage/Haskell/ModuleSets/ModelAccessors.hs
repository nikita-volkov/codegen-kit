module CodegenKit.ByLanguage.Haskell.ModuleSets.ModelAccessors
  ( moduleName,
    modules,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.BasePreludes as BasePreludesPackage
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.ModelAccessors.Templates as Templates
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.ModelTypes as ModelTypesPackage
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)

-- *

moduleName :: Name
moduleName =
  "accessors"

-- *

modules ::
  [(Text, Text, [(Text, Text, Int, Int)])] ->
  [(Text, [(Text, Text)])] ->
  Packaging.Modules
modules hasFieldConfigs hasVariantConfigs =
  Packaging.module_ True moduleName [] contentByNs
  where
    contentByNs ns =
      content
        (Snippets.moduleRef ns BasePreludesPackage.allName)
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

-- *

hasFieldDecls lcFieldName ucFieldName instanceConfigs =
  Templates.hasFieldClass lcFieldName ucFieldName :
  instanceDecls
  where
    instanceDecls =
      instanceConfigs
        & fmap
          ( \(hostName, fieldSig, fieldIndex, fieldAmount) ->
              Templates.hasFieldInstance ucFieldName hostName fieldSig fieldIndex fieldAmount
          )

hasVariantDecls ucVariantName instanceConfigs =
  Templates.hasVariantClass ucVariantName :
  instanceDecls
  where
    instanceDecls =
      instanceConfigs
        & fmap
          ( \(ucSumName, variantSig) ->
              Templates.hasVariantInstance ucVariantName ucSumName variantSig
          )
