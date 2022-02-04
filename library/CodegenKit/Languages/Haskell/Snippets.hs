module CodegenKit.Languages.Haskell.Snippets
  ( haddock,
    haddockWithNewline,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import CodegenKit.Prelude
import qualified TextBuilder as B'

haddock :: Text -> B.Builder
haddock = error "TODO"

haddockWithNewline :: Text -> B.Builder
haddockWithNewline =
  filtered (not . B.null) (flip mappend "\n") . haddock
