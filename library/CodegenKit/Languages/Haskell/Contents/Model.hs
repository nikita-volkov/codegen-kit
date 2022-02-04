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

record ::
  Text ->
  Text ->
  [(Text, Type)] ->
  [Deriving] ->
  Decl
record name haddock fields derivings =
  Decl
    [i|
      ${haddockCode}data $name
        = $name
    |]
  where
    haddockCode =
      Snippets.haddockWithNewline haddock

-- *

data Type

-- *

data Deriving
