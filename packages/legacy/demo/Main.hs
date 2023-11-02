module Main where

import Coalmine.Fileset qualified as Fileset
import Coalmine.Prelude
import CodegenKit.Legacy.ByLanguage.Haskell.Packaging qualified as HaskellPackaging
import Demo.SamplesFor.ModelAndUtils.Iso8601 qualified as Iso8601ModelAndUtils

-- * --

main :: IO ()
main =
  do
    Fileset.write . Fileset.inDir "demo-output" $ fileset
  where
    fileset =
      HaskellPackaging.toFileset
        "fmt-packs-formatica-iso8601-v1"
        "ISO-8601 model"
        (HaskellPackaging.listVersion 0 [1])
        (Just "nightly-2023-10-24")
        modules
      where
        modules =
          (HaskellPackaging.inNamespace ["fmt-packs", "formatica", "iso8601", "v1"] . mconcat)
            [ Iso8601ModelAndUtils.iso8601Modules
            ]
