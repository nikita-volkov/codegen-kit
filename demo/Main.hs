module Main where

import Coalmine.Prelude
import qualified CodegenKit.Languages.Haskell.Packages.ModelAndUtils as ModelAndUtils
import qualified CodegenKit.Packaging as Packaging

-- *

main =
  Packaging.print $
    mconcat
      [ modelAndUtilsPackage
      ]

-- *

modelAndUtilsPackage =
  ModelAndUtils.package
    ["our", "demo"]
    [ ModelAndUtils.product
        "ymd"
        ""
        []
    ]
    []
