module Tests.ByModule.ByLanguage.Haskell.Composers.Hashable where

import Coalmine.Prelude hiding (product)
import Coalmine.Tasty
import CodegenKit.ByLanguage.Haskell.Composers.Hashable
import CodegenKit.ByLanguage.Haskell.Composers.Module

tests =
  [ eqTestCase
      "Local product"
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
      ( compileModule
          "A"
          ["Prelude"]
          [("Data.Hashable", "Hashable")]
          (product "B" 3)
      ),
    eqTestCase
      "Imported product"
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
      ( compileModule
          "A"
          ["Prelude"]
          [ ("Data.Hashable", "Hashable"),
            ("Our.Namespace", "OurNamespace")
          ]
          (importing "Our.Namespace" "B" (\b -> product b 3))
      )
  ]
