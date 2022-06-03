-- |
-- API for immediately building every representation of the equals method that we need:
--
-- - The method declation
-- - The docs in any form
module CodegenKit.ByLanguage.Java.Builders.Equals
  ( -- * Execution
    equalsMethodsForProduct,

    -- * Field
    Field,
    primitiveField,
    objectField,
    arrayField,
  )
where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude
import qualified Data.HashSet as HashSet

-- * --

-- |
-- Build as an equals method for product.
equalsMethodsForProduct :: Text -> [Field] -> Builder
equalsMethodsForProduct className = \case
  [] ->
    [i|
      public boolean equals(Object that) {
        return that instanceof $className;
      }
    |]
  (multiFieldsExp -> fields) ->
    [i|
      public boolean equals(Object that) {
        return that instanceof $className && equals(($className) that);
      }
      private boolean equals($className that) {
        return
          $fields;
      }
    |]

-- * --

multiFieldsExp :: [Field] -> Builder
multiFieldsExp =
  List.intercalateMap fieldEqualsMethodExp " &&\n"

-- * --

data Field = Field
  { fieldEqualsMethodExp :: Builder
  }

primitiveField :: Text -> Field
primitiveField fieldName =
  Field
    [j|$fieldName == that.$fieldName|]

objectField :: Text -> Field
objectField fieldName =
  Field
    [j|$fieldName.equals(that.$fieldName)|]

arrayField :: Text -> Field
arrayField fieldName =
  Field
    [j|java.util.Arrays.equals($fieldName, that.$fieldName)|]
