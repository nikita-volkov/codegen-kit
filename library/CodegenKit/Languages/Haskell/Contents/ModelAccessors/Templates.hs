-- |
-- A local convention of this module is to avoid calling into other public
-- definitions from it.
-- This enforces the avoidance of complicated types,
-- yet still allows implementation of complex cases using function-local defs,
-- like using the \"where\" clause.
module CodegenKit.Languages.Haskell.Contents.ModelAccessors.Templates where

import Coalmine.Inter
import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)
import qualified TextBuilder as B'

module_ namespace productFieldsContent sumVariantsContent =
  [i|
    -- |
    -- Definitions for all patterns of access to members of all types defined in
    -- the "${namespace}.Model" module. E.g., setting a field value or mapping over a
    -- constructor type. 
    --
    -- This module completely resolves the \"records\" problem for all the types
    -- defined in the model. It also resolves the similar problem of access to
    -- members of the \"sum\" types.
    --
    -- On top of that all it provides definitions of Van Laarhoven optics which
    -- are directly compatible with the \"lens\" library and at the cost of no
    -- extra dependencies.
    --
    -- Our approach here is to provide classes for each name of the product field
    -- and the sum variant appearing in the model. Just those two types of classes
    -- is enough to integrate with any data-structure.
    module ${namespace}.ModelAccessors where

    import qualified ${namespace}.Model as M
    import ${namespace}.Prelude


    -- * Product Fields

    $productFieldsContent

    -- * Sum Variants
    
    $sumVariantsContent
  |]

hasFieldClass lcFieldName ucFieldName =
  [i|
    class Has${ucFieldName}Field a where
      type ${ucFieldName}FieldOf a
      get${ucFieldName}Field :: a -> ${ucFieldName}FieldOf a
      set${ucFieldName}Field :: ${ucFieldName}FieldOf a -> a -> a
      -- |
      -- Map over the \"$lcFieldName\" field of a record that has it.
      map${ucFieldName}Field ::
        (${ucFieldName}FieldOf a -> ${ucFieldName}FieldOf a) ->
        (a -> a)
      map${ucFieldName}Field map a =
        set${ucFieldName}Field (map (get${ucFieldName}Field a)) a
      -- |
      -- Update the \"${lcFieldName}\" field of a record @a@ in a context @f@.
      --
      -- This is a Van Laarhoven lens, meaning that it's identical to the
      -- following definition using the \"lens\" library:
      --
      -- > traverse${ucFieldName}Field ::
      -- >   (Has${ucFieldName}Field a) =>
      -- >   Lens' a (${ucFieldName}FieldOf a)
      --
      -- Thus it is directly compatible.
      traverse${ucFieldName}Field ::
        (Functor f) =>
        (${ucFieldName}FieldOf a -> f (${ucFieldName}FieldOf a)) ->
        (a -> f a)
      traverse${ucFieldName}Field traverse a =
        fmap (flip set${ucFieldName}Field a) (traverse (get${ucFieldName}Field a))
  |]

hasVariantClass ucVariantName =
  [i|
    class Has${ucVariantName}Variant a where
      type ${ucVariantName}VariantOf a
      lookup${ucVariantName}Variant :: a -> Maybe (${ucVariantName}VariantOf a)
      from${ucVariantName}Variant :: ${ucVariantName}VariantOf a -> a
      map${ucVariantName}Variant ::
        (${ucVariantName}VariantOf a -> ${ucVariantName}VariantOf a) ->
        (a -> a)
      map${ucVariantName}Variant map a =
        case lookup${ucVariantName}Variant a of
          Just b -> from${ucVariantName}Variant (map b)
          Nothing -> a
      traverse${ucVariantName}Variant ::
        (Has${ucVariantName}Variant a, Applicative f) =>
        (${ucVariantName}VariantOf a -> f (${ucVariantName}VariantOf a)) ->
        (a -> f a)
      traverse${ucVariantName}Variant traverse a =
        case lookup${ucVariantName}Variant a of
          Just b -> fmap from${ucVariantName}Variant (traverse b)
          Nothing -> pure a
  |]

hasFieldInstance ucFieldName ucProductName fieldTypeSig fieldIndex fieldsAmount =
  [i|
    instance Has${ucFieldName}Field ${ucProductName} where
      type ${ucFieldName}FieldOf ${ucProductName} = ${fieldTypeSig}
      get${ucFieldName}Field ($ucProductName$allFields) =
        $selectedFieldName
      set${ucFieldName}Field value ($ucProductName$allFields) =
        $setExp
      map${ucFieldName}Field map ($ucProductName$allFields) =
        $mapExp
      traverse${ucFieldName}Field traverse ($ucProductName$allFields) =
        fmap
          (\$selectedFieldName -> $ucProductName$allFields)
          (traverse $selectedFieldName)
  |]
  where
    -- Helpers
    fieldName n =
      "fld" <> B'.unsignedDecimal n
    constructorExp fieldExp =
      toMultilineTextBuilder ucProductName <> fields
      where
        fields =
          enumFromTo 0 (pred fieldsAmount)
            & fmap (\n -> if n == fieldIndex then fieldExp else B.uniline (fieldName (succ n)))
            & foldMap (mappend " ")
    -- Definitions
    allFields =
      enumFromTo 1 fieldsAmount
        & foldMap (mappend " " . fieldName)
        & B.uniline
    selectedFieldName =
      B.uniline $ fieldName (succ fieldIndex)
    setExp =
      constructorExp "value"
    mapExp =
      constructorExp ("(map " <> selectedFieldName <> ")")

hasVariantInstance ucVariantName ucSumName variantSig =
  [i|
    instance Has${ucVariantName}Variant ${ucSumName} where
      type ${ucVariantName}VariantOf ${ucSumName} = ${variantSig}
      lookup${ucVariantName}Variant a =
        case a of
          M.${ucVariantName}${ucSumName} b -> Just b
          _ -> Nothing
      from${ucVariantName}Variant = M.${ucVariantName}${ucSumName}
      map${ucVariantName}Variant map a =
        case a of
          M.${ucVariantName}${ucSumName} b ->
            M.${ucVariantName}${ucSumName} (map b)
          _ -> a
      traverse${ucVariantName}Variant traverse a =
        case a of
          M.${ucVariantName}${ucSumName} b ->
            fmap M.${ucVariantName}${ucSumName} (traverse b)
          _ -> pure a
  |]
