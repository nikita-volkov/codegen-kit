module CodegenKit.ByLanguage.Haskell.Composers.Hashable where

import Coalmine.MultilineTextBuilder qualified as Builder
import CodegenKit.ByLanguage.Haskell.Composers.Module
import CodegenKit.ByLanguage.Haskell.Snippets qualified as Snippets
import CodegenKit.Prelude hiding (intercalate)

product :: Text -> Int -> Body
product productRef fieldAmount =
  importing "Data.Hashable" "Hashable" $ \hashable ->
    importing "Data.Hashable" "hashWithSalt" $ \hashWithSalt ->
      let patterns =
            foldMap (mappend " ") fieldNames
          fieldNames =
            Snippets.enumAlphabeticNames fieldAmount
          definition =
            "salt" <> Builder.indent 2 (foldMap extendCode fieldNames)
            where
              extendCode name =
                [j|

                  & flip $hashWithSalt $name
                |]
       in splice
            [i|
              instance $hashable $productRef where
                hashWithSalt salt ($productRef$patterns) =
                  $definition
            |]
