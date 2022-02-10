module CodegenKit.Languages.Haskell.Packages.Commons
  ( -- *
    basePreludes,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.Languages.Haskell.Dependencies as Dependencies
import qualified CodegenKit.Languages.Haskell.Packaging as Packaging
import CodegenKit.Prelude hiding (Product, Sum, product, sum)
import qualified TextBuilder as B'

-- *

basePreludes :: Packaging.Modules
basePreludes =
  Packaging.inNamespace ["base-preludes"] . mconcat $
    [ baseOperators,
      baseDataTypes
    ]

baseOperators :: Packaging.Modules
baseOperators =
  Packaging.module_ False "operators" deps contents
  where
    deps =
      [ Dependencies.base
      ]
    contents nsNameList =
      [i|
        module $namespace.BasePreludes.Operators
          ( -- * From "Control.Applicative"
            (Control.Applicative.*>),
            (Control.Applicative.<*),
            (Control.Applicative.<*>),
            (Control.Applicative.<|>),

            -- * From "Control.Monad"
            (Control.Monad.<=<),
            (Control.Monad.=<<),
            (Control.Monad.>=>),
            (Control.Monad.>>),
            (Control.Monad.>>=),

            -- * From "Data.Bool"
            (Data.Bool.&&),
            (Data.Bool.||),
            (Data.Eq./=),
            (Data.Eq.==),

            -- * From "Data.Function"
            (Data.Function.$$),
            (Data.Function.&),
            (Data.Function..),

            -- * From "Data.Functor"
            (Data.Functor.$$>),
            (Data.Functor.<$$),
            (Data.Functor.<$$>),
            (Data.Functor.<&>),

            -- * From "Data.Ord"
            (Data.Ord.<),
            (Data.Ord.<=),
            (Data.Ord.>),
            (Data.Ord.>=),

            -- * From "Data.Ratio"
            (Data.Ratio.%),

            -- * From "Data.Semigroup"
            (Data.Semigroup.<>),

            -- * From "Prelude"
            (Prelude.$$!),
            (Prelude.*),
            (Prelude.+),
            (Prelude.-),
            (Prelude./),
            (Prelude.^),
            (Prelude.^^),
          )
        where

        import qualified Control.Applicative
        import qualified Control.Monad
        import qualified Data.Bool
        import qualified Data.Eq
        import qualified Data.Function
        import qualified Data.Functor
        import qualified Data.Ord
        import qualified Data.Ratio
        import qualified Data.Semigroup
        import qualified Prelude

      |]
      where
        namespace =
          B.uniline $ B'.intercalate "." $ fmap Name.toUpperCamelCaseTextBuilder nsNameList

baseDataTypes :: Packaging.Modules
baseDataTypes =
  Packaging.module_ False "data-types" deps contents
  where
    deps =
      [ Dependencies.base
      ]
    contents nsNameList =
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
        module $namespace.DataTypes
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
      where
        namespace =
          B.uniline $ B'.intercalate "." $ fmap Name.toUpperCamelCaseTextBuilder nsNameList
