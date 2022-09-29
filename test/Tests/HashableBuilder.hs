module Tests.HashableBuilder where

import Coalmine.Prelude hiding (product)
import Coalmine.Tasty
import CodegenKit.ByLanguage.Haskell.HashableBuilder
import CodegenKit.ByLanguage.Haskell.ModuleBuilder

tests =
  [ eqTestCase
      "product"
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
      )
  ]
