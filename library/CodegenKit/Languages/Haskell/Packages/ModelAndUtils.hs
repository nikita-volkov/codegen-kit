module CodegenKit.Languages.Haskell.Packages.ModelAndUtils where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Languages.Haskell.Contents.Model as Model
import qualified CodegenKit.Languages.Haskell.Contents.ModelAccessors as ModelAccessors
import CodegenKit.PackageAssembly
import CodegenKit.Prelude hiding (Product, Sum)
import qualified Data.Map.Strict as Map
import qualified TextBuilder as B'

-- *

package ::
  -- | Namespace.
  [TextBuilder] ->
  [Product] ->
  [Sum] ->
  Package
package ns products sums =
  mconcat
    [ m "Model" model,
      m "ModelAccessors" modelAccessors
    ]
  where
    m modName contents =
      fromFile
        (Paths.inDir packageModulesDir $ fromString $ modName <> ".hs")
        contents
    packageModulesDir =
      fromString . toString $ foldMap (flip mappend "/") ns
    packageNamespace =
      fromString . toString $ B'.intercalate "." ns
    model =
      Model.content packageNamespace sections
      where
        sections =
          [ Model.section "Product Types" productDecls,
            Model.section "Sum Types" sumDecls
          ]
          where
            productDecls =
              products & fmap mapper & join
              where
                mapper (Product productName productDocs fields fieldsAmount) =
                  Model.productAndInstances productName productDocs modelFields
                  where
                    modelFields =
                      fmap modelField fields
                      where
                        modelField (Field _ lcFieldName fieldDocs (MemberType sig _) _) =
                          (lcFieldName, fieldDocs, sig)
            sumDecls =
              error "TODO"
    modelAccessors =
      ModelAccessors.content
        packageNamespace
        hasFieldConfigs
        hasVariantConfigs
      where
        hasFieldConfigs =
          foldr step exit products Map.empty
          where
            step (Product productName productDocs fields fieldsAmount) next registry =
              foldr step next fields registry
              where
                step (Field ucFieldName lcFieldName fieldDocs (MemberType _ fieldSig) index) next registry =
                  next $! Map.alter alteration ucFieldName registry
                  where
                    alteration = \case
                      Nothing ->
                        Just (lcFieldName, [instanceConfig])
                      Just (lcFieldName, instanceConfigs) ->
                        Just (lcFieldName, instanceConfig : instanceConfigs)
                    instanceConfig =
                      (productName, fieldSig, index, fieldsAmount)
            exit ::
              Map Text (Text, [(Text, Text, Int, Int)]) ->
              [(Text, Text, [(Text, Text, Int, Int)])]
            exit =
              error "TODO"
        hasVariantConfigs =
          error "TODO"

-- *

data Product
  = Product
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      [Field]
      -- ^ Fields.
      Int
      -- ^ Precalculated amount of fields.

data Field
  = Field
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Lowercase name.
      Text
      -- ^ Docs.
      MemberType
      -- ^ Field type.
      Int
      -- ^ Field index.

data Sum
  = Sum
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      [Variant]
      -- ^ Variants.

data Variant
  = Variant
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      MemberType
      -- ^ Variant type.

data MemberType
  = MemberType
      Model.Type
      -- ^ Type for model.
      Text
      -- ^ Signature for accessors.
