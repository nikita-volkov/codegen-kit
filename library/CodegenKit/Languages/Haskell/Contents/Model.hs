module CodegenKit.Languages.Haskell.Contents.Model where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.Languages.Haskell.Snippets as Snippets
import CodegenKit.Prelude
import qualified TextBuilder as B'

-- *

content ::
  -- | Namespace.
  Text ->
  -- | Docs.
  Text ->
  -- | Declaration sections.
  [Section] ->
  Text
content =
  error "TODO"

-- *

data Section

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
