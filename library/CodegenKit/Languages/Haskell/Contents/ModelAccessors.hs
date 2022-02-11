module CodegenKit.Languages.Haskell.Contents.ModelAccessors
  ( content,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.Languages.Haskell.Contents.ModelAccessors.Templates as Templates
import CodegenKit.Prelude hiding (product, sum)

-- *

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
