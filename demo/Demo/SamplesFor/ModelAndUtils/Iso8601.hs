module Demo.SamplesFor.ModelAndUtils.Iso8601 where

import Coalmine.Inter
import CodegenKit.Languages.Haskell.Packages.ModelAndUtils

-- *

iso8601Package =
  package
    ["our", "demo"]
    [ section
        "ISO-8601"
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
            ],
          sum
            "calendar-date"
            [i|
              - `YYYY-MM-DD` or `YYYYMMDD`
              - `YYYY-MM` (but not `YYYYMM`)
              - `--MM-DD` or `--MMDD`
            |]
            [ variant "ymd" "" [modelType "Ymd"],
              variant "ym" "" [modelType "Ym"],
              variant "md" "" [modelType "Md"]
            ]
        ]
    ]
