module CodegenKit.Languages.Haskell.Dependencies where

import qualified CodegenKit.Languages.Haskell.Packaging as Packaging
import CodegenKit.Prelude

base, bytestring, containers, hashable, scientific, text, time, uuid, vector :: Packaging.Dependency
base =
  Packaging.dependency "base" 4 [12] 5 []
bytestring =
  Packaging.dependency "bytestring" 0 [10] 0 [12]
containers =
  Packaging.dependency "containers" 0 [6] 0 [7]
hashable =
  Packaging.dependency "hashable" 1 [3, 5] 1 [5]
scientific =
  Packaging.dependency "scientific" 0 [3] 0 [4]
text =
  Packaging.dependency "text" 1 [2] 3 []
time =
  Packaging.dependency "time" 1 [9] 2 []
uuid =
  Packaging.dependency "uuid" 1 [3] 2 []
vector =
  Packaging.dependency "vector" 0 [12] 0 [13]
