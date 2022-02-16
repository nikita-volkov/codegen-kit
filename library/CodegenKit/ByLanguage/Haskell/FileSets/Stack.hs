module CodegenKit.ByLanguage.Haskell.FileSets.Stack
  ( -- *
    fileSet,

    -- *
    ExtraDep,
    hackageExtraDep,
    githubExtraDep,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.Packaging as Packaging
import CodegenKit.Prelude hiding (Version)
import qualified Data.List.Extra as ListExtra
import qualified TextBuilder as B'

-- *

fileSet :: [ExtraDep] -> Packaging.FileSet
fileSet =
  Packaging.fromFile "stack.yaml" . contents

-- *

contents :: [ExtraDep] -> Text
contents extraDepList =
  [i|
    resolver: nightly-2022-02-16
    extra-deps:
      $extraDepsSplice
  |]
  where
    extraDepsSplice =
      B.intercalate "\n"
        . fmap extraDepSplice
        . ListExtra.nubSortOn extraDepName
        $ extraDepList

-- *

data ExtraDep = ExtraDep
  { -- | Name. For ordering.
    extraDepName :: Text,
    -- | Definition.
    extraDepSplice :: Builder
  }

hackageExtraDep :: Text -> Word -> [Word] -> ExtraDep
hackageExtraDep name versionHead versionTail =
  ExtraDep name splice
  where
    splice =
      B.uniline . mconcat $
        [ "- ",
          fromText name,
          "-",
          B'.unsignedDecimal versionHead,
          foldMap (mappend "." . B'.unsignedDecimal) versionTail
        ]

githubExtraDep :: Text -> Text -> Text -> Text -> ExtraDep
githubExtraDep name user repo commitHash =
  ExtraDep name splice
  where
    splice =
      [i|
        - git: https://github.com/$user/$repo
          commit: $commitHash
      |]
