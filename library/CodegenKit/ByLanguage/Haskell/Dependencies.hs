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
      [ githubStackExtraDep "coalmine" "nikita-volkov" "coalmine" "637c01270b3d2254124d24231d0b7a5c3e0db602",
        githubStackExtraDep "fx" "nikita-volkov" "fx" "f3947e32f8cc3c764ab8ee48581f2e1f35d3bcdb",
        githubStackExtraDep "moore-machines" "nikita-volkov" "moore-machines" "7e95c33b203a1cfe42dbc0e0de3cec8e6fe94290",
        hackageStackExtraDep "vector-extras" 0 [2, 6],
        hackageStackExtraDep "text-builder-dev" 0 [2],
        hackageStackExtraDep "yaml-unscrambler" 0 [1, 0, 8],
        hackageStackExtraDep "aeson-value-parser" 0 [19, 6]
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
