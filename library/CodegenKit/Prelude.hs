module CodegenKit.Prelude
  ( module Exports,
    filtered,
  )
where

import Coalmine.Inter as Exports (i)
import Coalmine.Name as Exports (Name)
import Coalmine.Prelude as Exports hiding (FilePath)
import Coalmine.SimplePaths as Exports (DirPath, FilePath)

-- *

filtered :: Monoid m => (a -> Bool) -> (a -> m) -> a -> m
filtered p k x
  | p x = k x
  | otherwise = mempty
