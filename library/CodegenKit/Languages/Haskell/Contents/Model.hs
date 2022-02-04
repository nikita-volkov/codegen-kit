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

      $content
    |]
  where
    haddockCode = Snippets.haddockWithNewline docs
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
    |]
  where
    haddockCode =
      Snippets.haddockWithNewline haddock
    fieldsCode =
      B.intercalate "\n" $ fmap fieldCode $ fields
      where
        fieldCode (_, Type typeCode) =
          "!" <> typeCode

-- *

newtype Type = Type B.Builder

-- *

preludeAlias :: Text
preludeAlias = "P"
