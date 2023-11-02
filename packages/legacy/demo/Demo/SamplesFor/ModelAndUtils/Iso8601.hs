module Demo.SamplesFor.ModelAndUtils.Iso8601 where

import Coalmine.Inter
import CodegenKit.Legacy.ByLanguage.Haskell.ModuleSets.ModelAndUtils
import CodegenKit.Legacy.ByLanguage.Haskell.Packaging (Modules)

-- * --

iso8601Modules :: Modules
iso8601Modules =
  modules
    [ section
        "ISO-8601"
        [ sum
            "calendar-date"
            [i|
              - @YYYY-MM-DD@ or @YYYYMMDD@
              - @YYYY-MM@ (but not @YYYYMM@)
              - @--MM-DD@ or @--MMDD@
            |]
            [ variant "ymd" "" [modelType "Ymd"],
              variant "ym" "" [modelType "Ym"],
              variant "md" "" [modelType "Md"]
            ],
          product
            "ymd"
            "ISO-8601 Year Month Day for representing a date."
            [ field
                "separted"
                "Whether the format contains dashes."
                (primitiveType "Bool"),
              field
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
          product
            "ym"
            [i|
              Year, month.

              @YYYY-MM@
            |]
            [ field
                "year"
                "Year."
                (primitiveType "Int"),
              field
                "month"
                "Month."
                (primitiveType "Int")
            ],
          product
            "md"
            [i|
              ISO-8601 Month Day for representing a date.

              - @--MM-DD@
              - @--MMDD@
            |]
            [ field
                "separted"
                "Whether the format contains dashes."
                (primitiveType "Bool"),
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
            "tz"
            "Timezone."
            [ variant "gmt" "GMT" [],
              variant "pst" "PST" [],
              variant "msk" "MSK" []
            ]
        ]
    ]
