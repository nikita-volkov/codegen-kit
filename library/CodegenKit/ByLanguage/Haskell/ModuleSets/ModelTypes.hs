module CodegenKit.ByLanguage.Haskell.ModuleSets.ModelTypes
  ( -- *
    moduleName,

    -- *
    modules,

    -- *
    Section,
    section,

    -- *
    Decl,
    productAndInstances,
    sumAndInstances,

    -- *
    Type,
    primitiveType,
    modelType,
    boxedVectorType,
    unboxedVectorType,
  )
where

import qualified Coalmine.List as List
import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.ByLanguage.Haskell.Dependencies as Dependencies
import qualified CodegenKit.ByLanguage.Haskell.ExpFormatter as ExpFormatter
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.BasePreludes as BasePreludesPackage
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (product, sum)
import qualified TextBuilder as B'

-- *

moduleName :: Name
moduleName = "types"

-- *

modules ::
  -- | Declaration sections.
  [Section] ->
  Packaging.Modules
modules sections =
  Packaging.module_ True moduleName deps (content sections)
  where
    deps =
      [ Dependencies.hashable,
        Dependencies.vector
      ]

-- *

content ::
  -- | Declaration sections.
  [Section] ->
  -- | Namespace
  [Name] ->
  Text
content sections ns =
  [i|
    module $moduleRef where

    import $operatorsPreludeRef
    import qualified $allPreludeRef as $preludeAlias
    import qualified Data.Hashable as $hashableAlias
    import qualified Data.Vector as $boxedVectorAlias
    import qualified Data.Vector.Unboxed as $unboxedVectorAlias

    $content
  |]
  where
    moduleRef =
      Snippets.moduleRef ns moduleName
    operatorsPreludeRef =
      Snippets.moduleRef ns BasePreludesPackage.operatorsName
    allPreludeRef =
      Snippets.moduleRef ns BasePreludesPackage.allName
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
        deriving ($derivings)
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
    derivings :: B.Builder
    derivings =
      if all (\(_, _, x) -> null x) variants
        then
          [i|
            $preludeAlias.Show, $preludeAlias.Eq, $preludeAlias.Ord, $preludeAlias.Enum, $preludeAlias.Bounded
          |]
        else
          [i|
            $preludeAlias.Show, $preludeAlias.Eq, $preludeAlias.Ord
          |]

-- * Product instances

productHashableInstance :: Text -> Int -> Decl
productHashableInstance productName fieldAmount =
  Decl
    [i|
      instance $hashableAlias.Hashable $productName where
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
      hashSaltExp fieldNames

productAccessorIsLabelInstance :: Text -> Text -> Type -> Int -> Int -> Decl
productAccessorIsLabelInstance productName fieldName (Type fieldType) fieldIndex fieldAmount =
  Decl
    [i|
      instance (a ~ $fieldType) => $preludeAlias.IsLabel "$fieldName" ($productName -> a) where
        fromLabel ($productName $fieldPatterns) =
          a
    |]
  where
    fieldPatterns =
      B.intercalate " " . mconcat $
        [ blanks 0 fieldIndex,
          ["a"],
          blanks (succ fieldIndex) fieldAmount
        ]
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

productTraverserIsLabelInstance productName fieldName (Type fieldType) fieldIndex fieldAmount =
  Decl
    [i|
      instance (a ~ $fieldType, $preludeAlias.Functor f) => $preludeAlias.IsLabel "$fieldName" ((a -> f a) -> $productName -> f $productName) where
        fromLabel traverse ($productName $varNames) =
          traverse $selectedVarName <&> \$selectedVarName -> $productName $varNames
    |]
  where
    varNames =
      varNamesFromUpTo 0 fieldAmount
        & B.intercalate " "
    selectedVarName =
      Snippets.alphabeticIndexName fieldIndex
    varNamesFromUpTo from to =
      enumFromTo from (pred to)
        & fmap Snippets.alphabeticIndexName

-- *

sumMapperIsLabelInstance :: Text -> Text -> Text -> Type -> Decl
sumMapperIsLabelInstance sumType variantName constructorName (Type variantType) =
  Decl
    [i|
      instance (a ~ $variantType) => $preludeAlias.IsLabel "$variantName" ((a -> a) -> $sumType -> $sumType) where
        fromLabel map sum = case sum of
          $constructorName a ->
            $constructorName (map a)
          _ ->
            sum
    |]

sumTraverserIsLabelInstance sumType variantName constructorName (Type variantType) =
  Decl
    [i|
      -- |
      -- Label which produces a function that updates the contents of
      -- the \"$variantName\" variant of '${sumType}' in an applicative context.
      -- 
      -- Exactly the same thing as the @Traversal@ from the \"lens\" library and is
      -- directly compatible with it.
      instance (a ~ $variantType, $preludeAlias.Applicative f) => $preludeAlias.IsLabel "$variantName" ((a -> f a) -> $sumType -> f $sumType) where
        fromLabel traverse sum = case sum of
          $constructorName a ->
            $constructorName <$$> traverse a
          _ ->
            $preludeAlias.pure sum
    |]

sumConstructorIsLabelInstance sumType variantName constructorName (Type variantType) =
  Decl
    [i|
      instance (a ~ $variantType) => $preludeAlias.IsLabel "$variantName" (a -> $sumType) where
        fromLabel =
          $constructorName
    |]

sumExtractorIsLabelInstance sumType variantName constructorName (Type variantType) =
  Decl
    [i|
      instance (a ~ $variantType) => $preludeAlias.IsLabel "$variantName" ($sumType -> $preludeAlias.Maybe a) where
        fromLabel sum = case sum of
          $constructorName a ->
            $preludeAlias.Just a
          _ ->
            $preludeAlias.Nothing
    |]

enumConstructorIsLabelInstance enumType variantName constructorName =
  Decl
    [i|
      instance $preludeAlias.IsLabel "$variantName" $enumType where
        fromLabel =
          $constructorName
    |]

enumPredicateIsLabelInstance enumType variantName constructorName =
  Decl
    [i|
      instance $preludeAlias.IsLabel "$variantName" ($enumType -> $preludeAlias.Bool) where
        fromLabel enum = case enum of
          $constructorName -> $preludeAlias.True
          _ -> $preludeAlias.False
    |]

sumHashableInstance :: Text -> [(Text, Int)] -> Decl
sumHashableInstance sumName variants =
  Decl
    [i|
      instance $hashableAlias.Hashable $sumName where
        hashWithSalt salt sum = case sum of
          $matches
    |]
  where
    matches =
      variants & zip (enumFrom 0) & fmap variantMatch & B.intercalate "\n"
      where
        variantMatch (variantIndex, (variantName, memberCount)) =
          [i|
            ${variantName}${sumName}${memberPatterns} ->
              $hashCode
          |]
          where
            variantIndexCode =
              B.uniline . mconcat $
                [ "(",
                  B'.decimal variantIndex,
                  " :: ",
                  toTextBuilder preludeAlias,
                  ".Int)"
                ]
            memberNames =
              enumFromTo 0 (pred memberCount)
                & fmap Snippets.alphabeticIndexName
            memberPatterns =
              foldMap (mappend " ") memberNames
            hashCode =
              hashSaltExp $
                variantIndexCode :
                memberNames

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
                productMapperIsLabelInstance productName name type_ i size,
                productTraverserIsLabelInstance productName name type_ i size
              ]
          )
        & join
    size = length fields

sumAndInstances ::
  Text ->
  Text ->
  [(Text, Text, Text, [Type])] ->
  [Decl]
sumAndInstances sumName sumDocs variants =
  typeDecl :
  hashableDecl :
  isLabelInstanceDecls
  where
    typeDecl =
      sum sumName sumDocs $ fmap fromVariant variants
      where
        fromVariant (_, ucVariantName, docs, memberTypes) =
          (ucVariantName, docs, memberTypes)
    hashableDecl =
      sumHashableInstance sumName $
        fmap fromVariant variants
      where
        fromVariant (_, name, _, members) =
          (name, length members)
    isLabelInstanceDecls =
      variants >>= variantInstances
      where
        variantInstances (lcVariantName, ucVariantName, docs, memberTypes) =
          case memberTypes of
            [memberType] ->
              [ sumMapperIsLabelInstance sumName lcVariantName constructorName memberType,
                sumTraverserIsLabelInstance sumName lcVariantName constructorName memberType,
                sumConstructorIsLabelInstance sumName lcVariantName constructorName memberType,
                sumExtractorIsLabelInstance sumName lcVariantName constructorName memberType
              ]
            [] ->
              [ enumConstructorIsLabelInstance sumName lcVariantName constructorName,
                enumPredicateIsLabelInstance sumName lcVariantName constructorName
              ]
            _ ->
              []
          where
            constructorName =
              ucVariantName <> sumName

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

-- * Helpers

preludeAlias :: B.Builder
preludeAlias = "P"

hashableAlias :: B.Builder
hashableAlias = "Hashable"

boxedVectorAlias :: B.Builder
boxedVectorAlias = "BVec"

unboxedVectorAlias :: B.Builder
unboxedVectorAlias = "UVec"

hashSaltExp extends =
  "salt" <> foldMap extendCode extends
  where
    extendCode extend =
      mconcat
        [ "\n  & ",
          preludeAlias,
          ".flip ",
          hashableAlias,
          ".hashWithSalt ",
          extend
        ]
