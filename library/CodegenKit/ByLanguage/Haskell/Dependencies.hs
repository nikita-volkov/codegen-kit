module CodegenKit.ByLanguage.Haskell.Dependencies where

import CodegenKit.ByLanguage.Haskell.Packaging
import CodegenKit.Prelude

base =
  dependency "base" 4 [12] 5 [] []

basePrelude =
  dependency "base-prelude" 1 [6] 2 [] []

bytestring =
  dependency "bytestring" 0 [10] 0 [12] []

coalmine =
  dependency "coalmine" 0 [1] 0 [2] stackDeps
  where
    stackDeps =
      [ githubStackExtraDep "coalmine" "nikita-volkov" "coalmine" "4241525781fb72b9b15f30932a44e45f6ab76442"
      ]

containers =
  dependency "containers" 0 [6] 0 [7] []

hashable =
  dependency "hashable" 1 [3, 5] 1 [5] []

scientific =
  dependency "scientific" 0 [3] 0 [4] []

text =
  dependency "text" 1 [2] 3 [] []

time =
  dependency "time" 1 [9] 2 [] []

uuid =
  dependency "uuid" 1 [3] 2 [] []

vector =
  dependency "vector" 0 [12] 0 [13] []
