-- | Dumb code snippets. No logic. Only care about layout.
module CodegenKit.ByLanguage.Java.Builders.HashCode.Snippets where

import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude
import qualified Data.Text as Text
import qualified TextBuilderDev

classHashStaticProperty :: Builder -> Builder
classHashStaticProperty className =
  [j|
    /**
     * Serves as the starting point for {@link $className#hashCode()}.
     */
    private final static int _classHash = $className.class.hashCode();
  |]

unitHashCodeMethod :: Builder
unitHashCodeMethod =
  [j|
    public int hashCode() {
      return _classHash;
    }
  |]

statementsHashCodeMethod :: [Builder] -> Builder
statementsHashCodeMethod statements =
  [j|
    public int hashCode() {
      int hash = _classHash;$statementsCode
      return hash;
    }
  |]
  where
    statementsCode =
      foldMap (mappend "\n") statements
