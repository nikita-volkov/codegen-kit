module CodegenKit.Languages.Haskell.Packages.ModelAndUtils
  ( -- *
    modules,

    -- *
    Section,
    section,

    -- *
    Decl,
    product,
    sum,

    -- *
    Field,
    field,

    -- *
    Variant,
    variant,

    -- *
    MemberType,
    primitiveType,
    modelType,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Languages.Haskell.Contents.ModelAccessors as ModelAccessors
import qualified CodegenKit.Languages.Haskell.Contents.ModelTypes as Model
import qualified CodegenKit.Languages.Haskell.Packages.Commons as CommonsPackage
import qualified CodegenKit.Languages.Haskell.Packaging as Packaging
import CodegenKit.Prelude hiding (Product, Sum, product, sum)
import qualified Data.Map.Strict as Map
import qualified TextBuilder as B'

-- *

modules ::
  -- | Namespace.
  [Name] ->
  [Section] ->
  Packaging.Modules
modules ns sections =
  mconcat
    [ CommonsPackage.basePreludes,
      Packaging.module_ True "types" deps modelContent,
      Packaging.module_ True "accessors" deps modelAccessorsContent
    ]
  where
    deps =
      [ Packaging.dependency "base" 4 [12] 5 [],
        Packaging.dependency "bytestring" 0 [10] 0 [12],
        Packaging.dependency "containers" 0 [6] 0 [7],
        Packaging.dependency "scientific" 0 [3] 0 [4],
        Packaging.dependency "text" 1 [2] 3 [],
        Packaging.dependency "time" 1 [9] 2 [],
        Packaging.dependency "uuid" 1 [3] 2 [],
        Packaging.dependency "vector" 0 [12] 0 [13]
      ]
    nsText =
      toText . B'.intercalate "." . fmap Name.toUpperCamelCaseTextBuilder
    modelContent ns =
      Model.content (nsText ns) $ fmap section sections
      where
        section (Section header decls) =
          Model.section header $ join $ fmap decl decls
          where
            decl = \case
              ProductDecl productName productDocs fields fieldsAmount ->
                Model.productAndInstances productName productDocs modelFields
                where
                  modelFields =
                    fmap modelField fields
                    where
                      modelField (FieldSpec _ lcFieldName fieldDocs (MemberType sig _) _) =
                        (lcFieldName, fieldDocs, sig)
              SumDecl sumName sumDocs variants ->
                Model.sumAndInstances sumName sumDocs $
                  fmap constructor variants
                where
                  constructor (Variant variantName variantDocs memberTypes) =
                    (variantName, variantDocs, fmap modelMemberType memberTypes)
                    where
                      modelMemberType (MemberType type_ _) =
                        type_
    modelAccessorsContent ns =
      ModelAccessors.content
        (nsText ns)
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
                  step (Variant ucVariantName variantDocs memberTypes) next variantRegistry =
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

-- *

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

-- *

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

-- *

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

-- *

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

-- *

data Variant
  = Variant
      Text
      -- ^ Uppercase name.
      Text
      -- ^ Docs.
      [MemberType]
      -- ^ Variant types.

variant :: Name -> Text -> [MemberType] -> Variant
variant name =
  Variant (Name.toUpperCamelCaseText name)

-- *

data MemberType
  = MemberType
      Model.Type
      -- ^ Type for model.
      Text
      -- ^ Signature for accessors.

-- |
-- Non-parametric type from Prelude.
primitiveType :: Text -> MemberType
primitiveType ucName =
  MemberType
    (Model.primitiveType ucName)
    ucName

modelType :: Text -> MemberType
modelType ucName =
  MemberType
    (Model.modelType ucName)
    ("M." <> ucName)
