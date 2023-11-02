module CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate where

import Coalmine.Prelude

data CodeStyle = CodeStyle
  { importQualifiedPost :: Bool,
    overloadedRecordDot :: Bool,
    strictData :: Bool
  }

-- | Class for parameters of templates.
class CodeTemplate a where
  compileCodeTemplate :: CodeStyle -> a -> Splice
