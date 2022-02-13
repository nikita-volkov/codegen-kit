module CodegenKit.ByLanguage.Haskell.Modules.BaseDataTypes
  ( -- *
    name,
    dependencies,
    content,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.ByLanguage.Haskell.Dependencies as Dependencies
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (all)
import qualified TextBuilder as B'

-- *

-- |
-- Module name.
name :: Text
name = "BaseDataTypes"

dependencies :: [Packaging.Dependency]
dependencies =
  [ Dependencies.base
  ]

-- *

content :: Text -> Text
content namespace =
  [i|
    -- |
    -- A module that reexports only the data types
    -- defined across various modules of the \"base\" package.
    --
    -- By data types we mean that it is the ones we use
    -- to define data structures.
    -- It is not abstraction integration wrappers,
    -- like 'Data.Semigroup.First'.
    -- It is not resource types like 'System.IO.Handle'.
    module $namespace.$name
      ( -- * From "Prelude"
        Prelude.Bool (..),
        Prelude.Char,
        Prelude.Double,
        Prelude.Either (..),
        Prelude.Float,
        Prelude.Integer,
        Prelude.Maybe (..),
        Prelude.String,

        -- * From "Data.Int"
        Data.Int.Int,
        Data.Int.Int16,
        Data.Int.Int32,
        Data.Int.Int64,
        Data.Int.Int8,

        -- * From "Data.Word"
        Data.Word.Word,
        Data.Word.Word16,
        Data.Word.Word32,
        Data.Word.Word64,
        Data.Word.Word8,

        -- * From "Data.Ratio"
        Data.Ratio.Rational,

        -- * From "Numeric.Natural"
        Numeric.Natural.Natural,

        -- * From "Data.List.NonEmpty"
        Data.List.NonEmpty.NonEmpty (..),
      )
    where

    import qualified Data.Int
    import qualified Data.List.NonEmpty
    import qualified Data.Ratio
    import qualified Data.Word
    import qualified Numeric.Natural
    import qualified Prelude

  |]
