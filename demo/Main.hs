module Main where

import Coalmine.Prelude
import qualified CodegenKit.Languages.Haskell.Packaging as HaskellPackaging
import qualified CodegenKit.Packaging as Packaging
import qualified Demo.SamplesFor.ModelAndUtils.Iso8601 as Iso8601ModelAndUtils

-- *

main =
  Packaging.print $
    HaskellPackaging.toFileSet
      "iso-8601"
      ""
      (HaskellPackaging.listVersion 0 [1])
      modules
  where
    modules =
      HaskellPackaging.inNamespace ["iso-8601"] $
        mconcat
          [ Iso8601ModelAndUtils.iso8601Modules
          ]
