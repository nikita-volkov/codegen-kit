module CodegenKit.ByLanguage.Haskell.ModuleSets.ModelAndUtils
  ( -- * --
    modules,

    -- * --
    Section,
    section,

    -- * --
    Decl,
    product,
    sum,
    alias,

    -- * --
    Field,
    field,

    -- * --
    Variant,
    variant,

    -- * --
    MemberType,
    primitiveType,
    modelType,
  )
where

import qualified Coalmine.BaseExtras.List as List
import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.ModelAccessors as ModelAccessorsPackage
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.ModelTypes as ModelTypesPackage
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (Product, Sum, product, sum)
import qualified Data.Map.Strict as Map
import qualified TextBuilderDev as B'

-- * --

modules :: [Section] -> Packaging.Modules
modules sections =
  mconcat
    [ modelTypes,
      modelAccessors
    ]
  where
    modelTypes =
      ModelTypesPackage.modules
        (fmap section sections)
      where
        section (Section header decls) =
          ModelTypesPackage.section header $ join $ fmap decl decls
          where
            decl = \case
              ProductDecl productName productDocs fields fieldsAmount ->
                ModelTypesPackage.productAndInstances productName productDocs modelFields
                where
                  modelFields =
                    fmap modelField fields
                    where
                      modelField (FieldSpec _ lcFieldName fieldDocs (MemberType sig _) _) =
                        (lcFieldName, fieldDocs, sig)
              SumDecl sumName sumDocs variants ->
                ModelTypesPackage.sumAndInstances sumName sumDocs $
                  fmap constructor variants
                where
                  constructor (Variant ucVariantName lcVariantName variantDocs memberTypes) =
                    (lcVariantName, ucVariantName, variantDocs, fmap modelMemberType memberTypes)
                    where
                      modelMemberType (MemberType type_ _) =
                        type_
              AliasDecl name docs (MemberType signature _) ->
                [ModelTypesPackage.alias name docs signature]
    modelAccessors =
      ModelAccessorsPackage.modules
        hasFieldConfigs
        hasVariantConfigs
      where
        decls = do
          Section _ decls <- sections
          decls
        (hasFieldConfigs, hasVariantConfigs) =
          foldr step exit decls Map.empty Map.empty
          where
            step decl next fieldRegistry variantRegistry = case decl of
              ProductDecl productName productDocs fields fieldsAmount ->
                foldr step exit fields fieldRegistry
                where
                  step (FieldSpec ucFieldName lcFieldName fieldDocs (MemberType _ fieldSig) index) next fieldRegistry =
                    next $! Map.alter alteration ucFieldName fieldRegistry
                    where
                      alteration = \case
                        Nothing ->
                          Just (lcFieldName, [instanceConfig])
                        Just (lcFieldName, instanceConfigs) ->
                          Just (lcFieldName, instanceConfig : instanceConfigs)
                      instanceConfig =
                        (productName, fieldSig, index, fieldsAmount)
                  exit fieldRegistry =
                    next fieldRegistry variantRegistry
              SumDecl sumName sumDocs variants ->
                foldr step exit variants variantRegistry
                where
                  step (Variant ucVariantName lcVariantName variantDocs memberTypes) next variantRegistry =
                    case memberTypes of
                      [] ->
                        -- TODO: Provide enum instances
                        next variantRegistry
                      [MemberType _ variantSig] ->
                        next $! Map.alter alteration ucVariantName variantRegistry
                        where
                          alteration = \case
                            Nothing ->
                              Just [instanceConfig]
                            Just instanceConfigs ->
                              Just $ instanceConfig : instanceConfigs
                          instanceConfig =
                            (sumName, variantSig)
                      _ ->
                        error "TODO: Multimember variant"
                  exit variantRegistry =
                    next fieldRegistry variantRegistry
              AliasDecl _ _ _ ->
                next fieldRegistry variantRegistry
            exit ::
              Map Text (Text, [(Text, Text, Int, Int)]) ->
              Map Text [(Text, Text)] ->
              ( [(Text, Text, [(Text, Text, Int, Int)])],
                [(Text, [(Text, Text)])]
              )
            exit fieldRegistry variantRegistry =
              (hasField, hasVariant)
              where
                hasField =
                  fmap
                    ( \(ucFieldName, (lcFieldName, instanceConfigs)) ->
                        (lcFieldName, ucFieldName, instanceConfigs)
                    )
                    . Map.toAscList
                    $ fieldRegistry
                hasVariant =
                  Map.toAscList variantRegistry

-- * --

data Section
  = Section
      Text
      -- ^ Header.
      [Decl]
      -- ^ Declarations.

section ::
  -- | Header.
  Text ->
  -- | Declarations.
  [Decl] ->
  Section
section =
  Section

-- * --

data Decl
  = ProductDecl
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      [FieldSpec]
      -- ^ Fields.
      Int
      -- ^ Precalculated amount of fields.
  | SumDecl
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      [Variant]
      -- ^ Variants.
  | AliasDecl
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      MemberType

product :: Name -> Text -> [Field] -> Decl
product name docs fields =
  ProductDecl
    (Name.toUpperCamelCaseText name)
    docs
    ( fields
        & zip (enumFrom 0)
        & fmap (\(index, Field fieldSpec) -> fieldSpec index)
    )
    (length fields)

sum :: Name -> Text -> [Variant] -> Decl
sum name =
  SumDecl
    (Name.toUpperCamelCaseText name)

alias :: Name -> Text -> MemberType -> Decl
alias name =
  AliasDecl
    (Name.toUpperCamelCaseText name)

-- * --

data FieldSpec
  = FieldSpec
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

-- * --

newtype Field
  = Field (Int -> FieldSpec)

field :: Name -> Text -> MemberType -> Field
field name docs type_ =
  Field $ \index ->
    FieldSpec
      (Name.toUpperCamelCaseText name)
      (Name.toLowerCamelCaseText name)
      docs
      type_
      index

-- * --

data Variant
  = Variant
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Lowercase name.
      Text
      -- ^ Docs.
      [MemberType]
      -- ^ Variant types.

variant :: Name -> Text -> [MemberType] -> Variant
variant name =
  Variant
    (Name.toUpperCamelCaseText name)
    (Name.toLowerCamelCaseText name)

-- * --

data MemberType
  = MemberType
      ModelTypesPackage.Type
      -- ^ Type for model.
      Text
      -- ^ Signature for accessors.

-- |
-- Non-parametric type from Prelude.
primitiveType :: Text -> MemberType
primitiveType ucName =
  MemberType
    (ModelTypesPackage.primitiveType ucName)
    ucName

modelType :: Text -> MemberType
modelType ucName =
  MemberType
    (ModelTypesPackage.modelType ucName)
    ("T." <> ucName)
