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
        module $namespace.Operators (module Exports) where

        import Prelude as Exports
          ( ($$),
            ($$>),
            (&),
            (*),
            (*>),
            (+),
            (-),
            (.),
            (/),
            (<),
            (<$$),
            (<$$>),
            (<&>),
            (<*),
            (<*>),
            (<=),
            (<>),
            (==),
            (>),
            (>=),
            (>>),
            (>>=),
          )
      |]
      where
        namespace =
          B.uniline $ B'.intercalate "." $ fmap Name.toUpperCamelCaseTextBuilder ns
