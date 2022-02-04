module CodegenKit.Module
  ( -- *
    Module (..),
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Prelude hiding (inDir, print)
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory

-- *

-- |
-- Module file-name and content generated given the namespace provided by the user.
newtype Module
  = -- |
    -- Function from namespace into filename and contents.
    Module ([Name] -> (FilePath, Text))
