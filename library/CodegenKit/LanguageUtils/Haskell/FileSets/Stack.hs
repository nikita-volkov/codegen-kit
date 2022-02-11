module CodegenKit.LanguageUtils.Haskell.FileSets.Stack
  ( -- *
    fileSet,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.Packaging as Packaging
import CodegenKit.Prelude hiding (Version)
import qualified TextBuilder as B'

-- *

fileSet :: Packaging.FileSet
fileSet =
  Packaging.fromFile "stack.yaml" contents

-- *

contents :: Text
contents =
  [i|
    resolver: nightly-2022-02-07
  |]
