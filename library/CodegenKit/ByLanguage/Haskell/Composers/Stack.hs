module CodegenKit.ByLanguage.Haskell.Composers.Stack
  ( -- * --
    fileSet,

    -- * --
    ExtraDep,
    hackageExtraDep,
    githubExtraDep,
  )
where

import qualified Coalmine.Fileset as Fileset
import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import CodegenKit.Prelude hiding (Version)
import qualified Data.List.Extra as ListExtra
import qualified TextBuilderDev as B'

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
