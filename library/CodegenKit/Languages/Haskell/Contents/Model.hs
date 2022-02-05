module CodegenKit.Languages.Haskell.Contents.Model where

import qualified Coalmine.MultilineTextBuilder as B
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude

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
section =
  error "TODO"

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

productIsLabelInstance :: Text -> Text -> Type -> Int -> Int -> Decl
productIsLabelInstance =
  error "TODO"

-- *

productAndInstances ::
  Text ->
  Text ->
  [(Text, Type)] ->
  [Decl]
productAndInstances =
  error "TODO"

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
