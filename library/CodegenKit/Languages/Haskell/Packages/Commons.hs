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
    contents ns =
      [i|
        module $namespace.Operators
          ( module Exports,
          )
        where

        import Control.Applicative as Exports
          ( (*>),
            (<*),
            (<*>),
            (<|>),
          )
        import Control.Monad as Exports
          ( (<=<),
            (=<<),
            (>=>),
            (>>),
            (>>=),
          )
        import Data.Eq as Exports
          ( (/=),
            (==),
          )
        import Data.Function as Exports
          ( ($$),
            (&),
            (.),
          )
        import Data.Functor as Exports
          ( ($$>),
            (<$$),
            (<$$>),
            (<&>),
          )
        import Data.Ord as Exports
          ( (<),
            (<=),
            (>),
            (>=),
          )
        import Data.Semigroup as Exports
          ( (<>),
          )
        import Prelude as Exports
          ( (*),
            (+),
            (-),
            (/),
          )

      |]
      where
        namespace =
          B.uniline $ B'.intercalate "." $ fmap Name.toUpperCamelCaseTextBuilder ns
