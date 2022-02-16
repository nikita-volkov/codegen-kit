-- |
-- A local convention of this module is to avoid calling into other public
-- definitions from it.
-- This enforces the avoidance of complicated types,
-- yet still allows implementation of complex cases using function-local defs,
-- like using the \"where\" clause.
module CodegenKit.ByLanguage.Haskell.ModuleSets.ModelAccessors.Templates where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)
import qualified TextBuilderDev as B'

module_ preludeModuleRef typesModuleRef moduleRef productFieldsContent sumVariantsContent =
  [i|
    -- |
    -- Definitions for all patterns of access to members of all types defined in
    -- the "${moduleRef}" module. E.g., setting a field value or mapping over a
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
    -- are enough to integrate with any data-structure.
    module ${moduleRef} where

    import qualified ${typesModuleRef} as T
    import ${preludeModuleRef}

    -- * Product Fields

    $productFieldsContent

    -- * Sum Variants
    
    $sumVariantsContent
  |]

hasFieldClass lcFieldName ucFieldName =
  [i|
    class Has${ucFieldName}Field a where
      -- |
      type ${ucFieldName}FieldOf a
      -- |
      get${ucFieldName}Field :: a -> ${ucFieldName}FieldOf a
      -- |
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
      -- |
      type ${ucVariantName}VariantOf a
      -- |
      lookup${ucVariantName}Variant :: a -> Maybe (${ucVariantName}VariantOf a)
      -- |
      from${ucVariantName}Variant :: ${ucVariantName}VariantOf a -> a
      -- |
      map${ucVariantName}Variant ::
        (${ucVariantName}VariantOf a -> ${ucVariantName}VariantOf a) ->
        (a -> a)
      map${ucVariantName}Variant map a =
        case lookup${ucVariantName}Variant a of
          Just b -> from${ucVariantName}Variant (map b)
          Nothing -> a
      -- |
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
    instance Has${ucFieldName}Field T.${ucProductName} where
      type ${ucFieldName}FieldOf T.${ucProductName} = ${fieldTypeSig}
      get${ucFieldName}Field (T.$ucProductName$allFields) =
        $selectedFieldName
      set${ucFieldName}Field value (T.$ucProductName$allFields) =
        $setExp
      map${ucFieldName}Field map (T.$ucProductName$allFields) =
        $mapExp
      traverse${ucFieldName}Field traverse (T.$ucProductName$allFields) =
        fmap
          (\$selectedFieldName -> T.$ucProductName$allFields)
          (traverse $selectedFieldName)
  |]
  where
    -- Helpers
    constructorExp fieldExp =
      "T." <> toMultilineTextBuilder ucProductName <> fields
      where
        fields =
          enumFromTo 0 (pred fieldsAmount)
            & fmap (\n -> if n == fieldIndex then fieldExp else Snippets.alphabeticIndexName n)
            & foldMap (mappend " ")
    -- Definitions
    allFields =
      enumFromTo 0 (pred fieldsAmount)
        & foldMap (mappend " " . Snippets.alphabeticIndexName)
    selectedFieldName =
      Snippets.alphabeticIndexName fieldIndex
    setExp =
      constructorExp "value"
    mapExp =
      constructorExp ("(map " <> selectedFieldName <> ")")

hasVariantInstance ucVariantName ucSumName variantSig =
  [i|
    instance Has${ucVariantName}Variant T.${ucSumName} where
      type ${ucVariantName}VariantOf T.${ucSumName} = ${variantSig}
      lookup${ucVariantName}Variant a =
        case a of
          T.${ucVariantName}${ucSumName} b -> Just b
          _ -> Nothing
      from${ucVariantName}Variant = T.${ucVariantName}${ucSumName}
      map${ucVariantName}Variant map a =
        case a of
          T.${ucVariantName}${ucSumName} b ->
            T.${ucVariantName}${ucSumName} (map b)
          _ -> a
      traverse${ucVariantName}Variant traverse a =
        case a of
          T.${ucVariantName}${ucSumName} b ->
            fmap T.${ucVariantName}${ucSumName} (traverse b)
          _ -> pure a
  |]
