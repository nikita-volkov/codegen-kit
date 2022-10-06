{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Composer DSL for Haddock.
module CodegenKit.ByLanguage.Haskell.Composers.Haddock where

import qualified Coalmine.ContainersExtras.Seq as Seq
import Coalmine.MultilineTextBuilder (Builder)
import CodegenKit.Prelude
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified TextBuilderDev as TextBuilder

compileHaddock :: [Block] -> TextBuilder
compileHaddock =
  error "TODO"

-- * --

newtype Block = Block
  { builder :: Builder
  }
  deriving (Semigroup, Monoid, IsString)

textSplice :: Text -> Block
textSplice = error "TODO"

prependEachLineInSplice :: Line -> Block -> Block
prependEachLineInSplice (Line line) (Block lines) =
  error "TODO"

newtype Line
  = Line TextBuilder
  deriving (Semigroup, Monoid)

instance IsString Line where
  fromString = fromMaybe (error "Not single-line") . textLine . fromString

-- | Line of text validated not to contain line-breaks.
textLine :: Text -> Maybe Line
textLine =
  error "TODO"

unsafeTextLine :: Text -> Line
unsafeTextLine =
  error "TODO"

indentationLine :: Int -> Line
indentationLine =
  error "TODO"
