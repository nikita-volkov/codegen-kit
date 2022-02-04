module Tests.HaskellExpFormatter where

import Coalmine.Inter
import Coalmine.Prelude
import Coalmine.Tasty
import CodegenKit.HaskellExpFormatter

tests =
  [ testCase "" $ do
      let expected =
            "( aa\n    <* [ bbb,\n         cc <* ddd\n       ]\n)"
          actual =
            toText $
              groupedExp
                ( infixBinOp
                    "<*"
                    (reference "" "aa")
                    ( multilineList
                        [ reference "" "bbb",
                          infixBinOp "<*" (reference "" "cc") (reference "" "ddd")
                        ]
                    )
                )
       in assertEqual "" expected actual,
    testCase "stringLiteral" $
      assertEqual
        ""
        "\"a\\n\\\n\\b\""
        (toText (groupedExp (stringLiteral "a\nb")))
  ]
