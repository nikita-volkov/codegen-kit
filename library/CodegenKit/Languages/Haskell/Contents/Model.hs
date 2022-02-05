module CodegenKit.Languages.Haskell.Contents.Model where

import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)

-- *

content ::
  -- | Namespace.
  Text ->
  -- | Docs.
  Text ->
  -- | Prelude module ref.
  Text ->
  -- | Declaration sections.
  [Section] ->
  Text
content namespace docs prelude sections =
  toText
    [i|
      ${haddockCode}module $namespace where

      import qualified $prelude as $preludeAlias
      import qualified Data.Vector as $boxedVectorAlias
      import qualified Data.Vector.Unboxed as $unboxedVectorAlias

      $content
    |]
  where
    haddockCode = Snippets.prefixHaddockWithNewline docs
    content =
      sections
        & coerce
        & B.intercalate "\n\n"

-- *

newtype Section = Section B.Builder

section ::
  -- | Heading.
  Text ->
  -- | Declarations.
  [Decl] ->
  Section
section heading declarations =
  Section
    [i|
      -- * $heading

      $declarationsCode
    |]
  where
    declarationsCode =
      B.intercalate "\n\n" $ coerce declarations

-- *

newtype Decl = Decl B.Builder

product ::
  Text ->
  Text ->
  [(Text, Type)] ->
  Decl
product name haddock fields =
  Decl
    [i|
      ${haddockCode}data $name
        = $name
            $fieldsCode
        deriving ($preludeAlias.Show, $preludeAlias.Eq, $preludeAlias.Ord, $preludeAlias.Generic)
    |]
  where
    haddockCode =
      Snippets.prefixHaddockWithNewline haddock
    fieldsCode =
      B.intercalate "\n" $ fmap fieldCode $ fields
      where
        fieldCode (docs, Type typeCode) =
          "!" <> typeCode <> Snippets.suffixHaddockWithNewline docs

productAccessorIsLabelInstance :: Text -> Text -> Type -> Int -> Int -> Decl
productAccessorIsLabelInstance productName fieldName (Type fieldType) fieldIndex fieldAmount =
  Decl
    [i|
      instance (a ~ $fieldType) => $preludeAlias.IsLabel "$fieldName" ($productName -> a) where
        fromLabel ($productName $fieldPatterns) = a
    |]
  where
    fieldPatterns =
      B.intercalate " " $
        blanks 0 (pred fieldIndex) <> ["a"] <> blanks (succ fieldIndex) (pred fieldAmount)
      where
        blanks from to =
          replicate (to - from) "_"

productMapperIsLabelInstance :: Text -> Text -> Type -> Int -> Int -> Decl
productMapperIsLabelInstance productName fieldName (Type fieldType) fieldIndex fieldAmount =
  Decl
    [i|
      instance (a ~ $fieldType) => $preludeAlias.IsLabel "$fieldName" ((a -> a) -> $productName -> $productName) where
        fromLabel map ($productName $fieldPatterns) =
          $productName $fieldExps
    |]
  where
    fieldPatterns =
      varNamesFromUpTo 0 fieldAmount
        & B.intercalate " "
    fieldExps =
      B.intercalate " " $
        varNamesFromUpTo 0 fieldIndex
          <> ["(map " <> Snippets.alphabeticIndexName fieldIndex <> ")"]
          <> varNamesFromUpTo (succ fieldIndex) fieldAmount
    varNamesFromUpTo from to =
      enumFromTo from (pred to)
        & fmap Snippets.alphabeticIndexName

-- *

productAndInstances ::
  Text ->
  Text ->
  [(Text, Text, Type)] ->
  [Decl]
productAndInstances productName productDocs fields =
  typeDecl :
  isLabelInstanceDecls
  where
    typeDecl =
      fields
        & fmap (\(name, docs, type_) -> (docs, type_))
        & product productName productDocs
    isLabelInstanceDecls =
      fields
        & zip (enumFrom 0)
        & fmap
          ( \(i, (name, docs, type_)) ->
              [ productAccessorIsLabelInstance productName name type_ i size,
                productMapperIsLabelInstance productName name type_ i size
              ]
          )
        & join
    size = length fields

-- *

newtype Type = Type B.Builder

primitiveType :: Text -> Type
primitiveType =
  Type . mappend preludeAlias . mappend "." . fromText

modelType :: Text -> Type
modelType =
  Type . fromText

boxedVectorType :: Type -> Type
boxedVectorType =
  Type . mappend boxedVectorAlias . mappend ".Vector " . coerce

unboxedVectorType :: Type -> Type
unboxedVectorType =
  Type . mappend unboxedVectorAlias . mappend ".Vector " . coerce

-- *

preludeAlias :: B.Builder
preludeAlias = "P"

boxedVectorAlias :: B.Builder
boxedVectorAlias = "BVec"

unboxedVectorAlias :: B.Builder
unboxedVectorAlias = "UVec"
