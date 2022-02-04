module Main where

import Coalmine.Tasty
import qualified Tests.HaskellExpFormatter as HaskellExpFormatter
import Prelude

main =
  defaultMain . testGroup "All" $
    [testGroup "HaskellExpFormatter" HaskellExpFormatter.tests]
