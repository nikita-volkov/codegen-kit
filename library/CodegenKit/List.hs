module CodegenKit.List where

import CodegenKit.Prelude

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd amount =
  reverse . drop amount . reverse
