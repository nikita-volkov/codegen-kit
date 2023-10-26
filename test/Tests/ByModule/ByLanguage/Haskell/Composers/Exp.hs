module Tests.ByModule.ByLanguage.Haskell.Composers.Exp where

import Coalmine.Prelude
import Coalmine.Tasty
import CodegenKit.ByLanguage.Haskell.Composers.Exp

tests =
  [ testCase "" $ do
      let expected =
            "( aa\n    <* [ bbb,\n         cc <* ddd\n       ]\n)"
          actual =
            to @Text
              $ groupedExp
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
    testCase "stringLiteral"
      $ assertEqual
        ""
        "\"a\\n\\\n\\b\""
        (to @Text (groupedExp (stringLiteral "a\nb")))
  ]
