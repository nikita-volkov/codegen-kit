module Main where

import Coalmine.Prelude
import qualified CodegenKit.Packaging as Packaging
import qualified Demo.SamplesFor.ModelAndUtils.Iso8601 as Iso8601ModelAndUtils

-- *

main =
  Packaging.print $
    mconcat
      [ Iso8601ModelAndUtils.iso8601FileSet
      ]
