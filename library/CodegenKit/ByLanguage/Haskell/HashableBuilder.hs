module CodegenKit.ByLanguage.Haskell.HashableBuilder where

import qualified Coalmine.MultilineTextBuilder as Builder
import CodegenKit.ByLanguage.Haskell.ModuleBuilder
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (intercalate)

product :: Body -> Int -> Body
product productRef fieldAmount =
  "instance "
    <> ref "Data.Hashable" "Hashable"
    <> " "
    <> productRef
    <> " where"
    <> indent
      2
      ( "\nhashWithSalt salt ("
          <> productRef
          <> foldMap (mappend " ") fieldNames
          <> ") ="
          <> indent 2 ("\n" <> definition)
      )
  where
    fieldNames =
      Snippets.enumAlphabeticNames fieldAmount
        & fmap splice
    definition =
      "salt" <> indent 2 (foldMap extendCode fieldNames)
      where
        extendCode name =
          mconcat
            [ "\n& flip ",
              ref "Data.Hashable" "hashWithSalt",
              " ",
              name
            ]
