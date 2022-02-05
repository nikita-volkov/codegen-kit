module Demo.SamplesFor.ModelAndUtils.Iso8601 where

import CodegenKit.Languages.Haskell.Packages.ModelAndUtils

-- *

iso8601Package =
  package
    ["our", "demo"]
    [ product
        "ymd"
        "ISO-8601 Year Month Day for representing a date."
        [ field
            "year"
            "Year."
            (primitiveType "Int"),
          field
            "month"
            "Month."
            (primitiveType "Int"),
          field
            "day"
            "Day."
            (primitiveType "Int")
        ]
    ]
    []
