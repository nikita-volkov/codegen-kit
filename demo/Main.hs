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
        "ISO-8601 Year Month Day for representing a date."
        [ ModelAndUtils.field
            "year"
            "Year."
            (ModelAndUtils.primitiveType "Int"),
          ModelAndUtils.field
            "month"
            "Month."
            (ModelAndUtils.primitiveType "Int"),
          ModelAndUtils.field
            "day"
            "Day."
            (ModelAndUtils.primitiveType "Int")
        ]
    ]
    []
