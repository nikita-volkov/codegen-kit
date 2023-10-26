module CodegenKit.ByLanguage.Haskell.Modules.BaseOperators
  ( -- * --
    name,
    dependencies,
    content,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.PackagingPresets.Dependencies as Dependencies
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (all)
import qualified TextBuilderDev as B'

-- * --

-- |
-- Module name.
name :: Text
name = "BaseOperators"

dependencies :: [Packaging.Dependency]
dependencies =
  [ Dependencies.base
  ]

-- * --

content :: Text -> Text
content namespace =
  [i|
    -- |
    -- A collection of common operators provided across
    -- various modules of the \"base\" package.
    module $namespace.$name
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

        -- * From "Data.Bits"
        (Data.Bits..&.),
        (Data.Bits..|.),

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
    import qualified Data.Bits
    import qualified Data.Bool
    import qualified Data.Eq
    import qualified Data.Function
    import qualified Data.Functor
    import qualified Data.Ord
    import qualified Data.Ratio
    import qualified Data.Semigroup
    import qualified Prelude

  |]
