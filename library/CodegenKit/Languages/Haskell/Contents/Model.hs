module CodegenKit.Languages.Haskell.Contents.Model where

import qualified Coalmine.List as List
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.HaskellExpFormatter as ExpFormatter
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)

-- *

content ::
  -- | Namespace.
  Text ->
  -- | Declaration sections.
  [Section] ->
  Text
content namespace sections =
  [i|
    module $namespace.Model where

    import $namespace.Operators
    import qualified $namespace.Prelude as $preludeAlias
    import qualified Data.Vector as $boxedVectorAlias
    import qualified Data.Vector.Unboxed as $unboxedVectorAlias

    $content
  |]
  where
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

-- * Type declarations

product ::
  Text ->
  Text ->
  [(Text, Type)] ->
  Decl
product name haddock fields =
  Decl
    [i|
      ${haddockPrefix}data $name
        = $name
            $fieldsCode
        deriving ($preludeAlias.Show, $preludeAlias.Eq, $preludeAlias.Ord)
    |]
  where
    haddockPrefix =
      Snippets.prefixHaddockWithNewline haddock
    fieldsCode =
      B.intercalate "\n" $ fmap fieldCode $ fields
      where
        fieldCode (docs, Type typeCode) =
          "!" <> typeCode <> Snippets.suffixHaddockWithNewline docs

sum :: Text -> Text -> [(Text, Text, [Type])] -> Decl
sum sumName haddock variants =
  Decl
    [i|
      ${haddockPrefix}data $sumName
        $constructors
        deriving ($preludeAlias.Show, $preludeAlias.Eq, $preludeAlias.Ord)
    |]
  where
    haddockPrefix =
      Snippets.prefixHaddockWithNewline haddock
    constructors =
      B.intercalate "\n" $
        List.mapHeadAndTail (variantCode "= ") (fmap (variantCode "| ")) $
          variants
      where
        variantCode prefix (ucVariantName, docs, memberTypes) =
          B.indent 2 $
            mconcat
              [ prefix,
                Snippets.prefixHaddockWithNewline docs,
                B.indent 2 $
                  mconcat
                    [ fromText ucVariantName,
                      fromText sumName,
                      foldMap memberCode memberTypes
                    ]
              ]
          where
            memberCode (Type typeCode) = " !" <> typeCode

-- * Product instances

productHashableInstance :: Text -> Int -> Decl
productHashableInstance productName fieldAmount =
  Decl
    [i|
      instance $preludeAlias.Hashable $productName where
        hashWithSalt salt ($productName $fieldPatterns) =
          $exp
    |]
  where
    fieldNames =
      enumFromTo 0 (pred fieldAmount)
        & fmap Snippets.alphabeticIndexName
    fieldPatterns =
      fieldNames
        & B.intercalate " "
    exp =
      ExpFormatter.ungroupedExp $
        ExpFormatter.multilinePostAppChain (ExpFormatter.reference "" "salt") $
          fmap fieldExp fieldNames
      where
        fieldExp name =
          ExpFormatter.appChain
            (ExpFormatter.reference (toText preludeAlias) "flip")
            [ ExpFormatter.reference (toText preludeAlias) "hashWithSalt",
              ExpFormatter.reference "" (toText name)
            ]

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
  productHashableInstance productName size :
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

sumAndInstances ::
  Text ->
  Text ->
  [(Text, Text, [Type])] ->
  [Decl]
sumAndInstances sumName sumDocs variants =
  typeDecl :
  []
  where
    typeDecl =
      sum sumName sumDocs variants

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
