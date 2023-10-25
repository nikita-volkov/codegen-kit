module Main where

import qualified Coalmine.Fileset as Fileset
import Coalmine.Prelude
import qualified CodegenKit.ByLanguage.Haskell.ModuleSets.BasePreludes as BasePreludesPackage
import qualified CodegenKit.ByLanguage.Haskell.Packaging as HaskellPackaging
import qualified Demo.SamplesFor.ModelAndUtils.Iso8601 as Iso8601ModelAndUtils

-- * --

main =
  do
    Fileset.write . Fileset.inDir "demo-output" $ fileset
  where
    fileset =
      HaskellPackaging.toFileset
        "fmt-packs-formatica-iso8601-v1"
        "ISO-8601 model"
        (HaskellPackaging.listVersion 0 [1])
        modules
      where
        modules =
          (HaskellPackaging.inNamespace ["fmt-packs", "formatica", "iso8601", "v1"] . mconcat)
            [ Iso8601ModelAndUtils.iso8601Modules
            ]
