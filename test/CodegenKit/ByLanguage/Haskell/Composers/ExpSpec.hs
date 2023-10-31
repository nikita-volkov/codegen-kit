module CodegenKit.ByLanguage.Haskell.Composers.ExpSpec where

import Coalmine.Prelude
import CodegenKit.ByLanguage.Haskell.Composers.Exp
import Test.Hspec

spec = do
  describe "multilineList" do
    it "Matches the reference value" do
      shouldBe
        ( to @Text
            ( groupedExp
                ( infixBinOp
                    "<*"
                    (reference "" "aa")
                    ( multilineList
                        [ reference "" "bbb",
                          infixBinOp "<*" (reference "" "cc") (reference "" "ddd")
                        ]
                    )
                )
            )
        )
        "( aa\n    <* [ bbb,\n         cc <* ddd\n       ]\n)"
  describe "stringLiteral" do
    it "Matches the reference value" do
      shouldBe
        (to @Text (groupedExp (stringLiteral "a\nb")))
        "\"a\\n\\\n\\b\""
