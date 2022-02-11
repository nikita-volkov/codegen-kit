module Main where

import Coalmine.Prelude
import qualified CodegenKit.Languages.Haskell.Packaging as HaskellPackaging
import qualified CodegenKit.Packaging as Packaging
import qualified Demo.SamplesFor.ModelAndUtils.Iso8601 as Iso8601ModelAndUtils

-- *

main =
  do
    Packaging.write . Packaging.inDir "demo-output" $ fileSet
  where
    fileSet =
      HaskellPackaging.toFileSet
        "iso8601"
        "ISO-8601 model"
        (HaskellPackaging.listVersion 0 [1])
        modules
      where
        modules =
          HaskellPackaging.inNamespace [] . mconcat $
            [ Iso8601ModelAndUtils.iso8601Modules
            ]
