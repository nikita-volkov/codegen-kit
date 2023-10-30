module CodegenKit.ByLanguage.Haskell.CodeTemplate where

import Coalmine.Prelude

data CodeStyle = CodeStyle
  { importQualifiedPost :: Bool
  }

class CodeTemplate a where
  codeSplice :: CodeStyle -> a -> Splice
