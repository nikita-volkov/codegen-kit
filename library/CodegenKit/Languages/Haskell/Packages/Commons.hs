module CodegenKit.Languages.Haskell.Packages.Commons
  ( -- *
    operators,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.Languages.Haskell.Dependencies as Dependencies
import qualified CodegenKit.Languages.Haskell.Packaging as Packaging
import CodegenKit.Prelude hiding (Product, Sum, product, sum)
import qualified TextBuilder as B'

-- *

operators :: Packaging.Modules
operators =
  Packaging.module_ False "Operators" deps contents
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
