module CodegenKit.Legacy.ByLanguage.Haskell.Composers.HashableSpec where

import Coalmine.Prelude hiding (product)
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Hashable
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Module
import Test.Hspec

spec :: Spec
spec = do
  describe "Local product" do
    it "Should print as expected"
      $ shouldBe
        ( compileModule
            "A"
            ["Prelude"]
            [("Data.Hashable", "Hashable")]
            (product "B" 3)
        )
        [i|
        module A where
          
        import Prelude
        import qualified Data.Hashable as Hashable

        instance Hashable.Hashable B where
          hashWithSalt salt (B a b c) =
            salt
              & flip Hashable.hashWithSalt a
              & flip Hashable.hashWithSalt b
              & flip Hashable.hashWithSalt c
      |]

  describe "Imported product" do
    it "Should print as expected"
      $ shouldBe
        ( compileModule
            "A"
            ["Prelude"]
            [ ("Data.Hashable", "Hashable"),
              ("Our.Namespace", "OurNamespace")
            ]
            (importing "Our.Namespace" "B" (\b -> product b 3))
        )
        [i|
        module A where
          
        import Prelude
        import qualified Data.Hashable as Hashable
        import qualified Our.Namespace as OurNamespace

        instance Hashable.Hashable OurNamespace.B where
          hashWithSalt salt (OurNamespace.B a b c) =
            salt
              & flip Hashable.hashWithSalt a
              & flip Hashable.hashWithSalt b
              & flip Hashable.hashWithSalt c
      |]
