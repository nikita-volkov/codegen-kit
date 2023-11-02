module CodegenKit.Legacy.ByLanguage.Haskell.Composers.Stack
  ( -- * --
    fileSet,

    -- * --
    ExtraDep,
    hackageExtraDep,
    githubExtraDep,
  )
where

import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder (Builder)
import Coalmine.MultilineTextBuilder qualified as B
import CodegenKit.Legacy.Prelude
import Data.List.Extra qualified as ListExtra
import TextBuilderDev qualified as B'

-- * --

fileSet :: Text -> [ExtraDep] -> Fileset.Fileset
fileSet resolver =
  Fileset.file "stack.yaml" . contents resolver

-- * --

contents :: Text -> [ExtraDep] -> Text
contents resolver extraDepList =
  [i|
    resolver: $resolver
    extra-deps:
      $extraDepsSplice
  |]
  where
    extraDepsSplice =
      B.intercalate "\n"
        . fmap extraDepSplice
        . ListExtra.nubSortOn extraDepName
        $ extraDepList

-- * --

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
      (B.uniline . mconcat)
        [ "- ",
          from @Text name,
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
