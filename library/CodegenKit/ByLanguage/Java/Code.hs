-- | Dump code snippets. No logic. Only care about layout.
module CodegenKit.ByLanguage.Java.Code where

import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude
import qualified Data.Text as Text
import qualified TextBuilderDev

type Code = Builder

-- * Hashing

classHashStaticProperty :: Code -> Code
classHashStaticProperty className =
  [j|
    /**
     * Serves as the starting point for {@link $className#hashCode()}.
     */
    private final static int classHash = $className.class.hashCode();
  |]

unitHashCodeMethod :: Code
unitHashCodeMethod =
  [j|
    public int hashCode() {
      return classHash;
    }
  |]

-- * --

fieldParamDoc :: Code -> Code -> Code
fieldParamDoc className fieldName =
  [j|
    * @param $fieldName value of the {@code $fieldName} property of
    *        $fieldNameSpace the {@code $className} case.
  |]
  where
    fieldNameSpace =
      flip Text.replicate " " . TextBuilderDev.length . to $ fieldName

memberlessEqualsMethod :: Code -> Code
memberlessEqualsMethod className =
  [j|
    public boolean equals(Object that) {
      return that instanceof $className;
    }
  |]

productEqualsMethods :: Code -> Code -> Code
productEqualsMethods className equalityExprs =
  [j|
    public boolean equals(Object that) {
      return that instanceof $className && equals(($className) that);
    }
    private boolean equals($className that) {
      return
        $equalityExprs;
    }
  |]
