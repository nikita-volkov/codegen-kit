-- | Dumb code snippets. No logic. Only care about layout.
module CodegenKit.ByLanguage.Java.Builders.HashCode.Snippets where

import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude

unitHashCodeMethod :: Builder -> Builder
unitHashCodeMethod className =
  [j|
    public int hashCode() {
      return $className.class.hashCode();
    }
  |]

statementsHashCodeMethod :: Builder -> [Builder] -> Builder
statementsHashCodeMethod className statements =
  [j|
    public int hashCode() {
      int hash = $className.class.hashCode();$statementsCode
      return hash;
    }
  |]
  where
    statementsCode =
      foldMap (mappend "\n") statements
