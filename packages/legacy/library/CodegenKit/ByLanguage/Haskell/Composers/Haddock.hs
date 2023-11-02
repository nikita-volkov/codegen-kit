{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Composer DSL for Haddock.
module CodegenKit.ByLanguage.Haskell.Composers.Haddock where

import Coalmine.MultilineTextBuilder (Builder)
import CodegenKit.Prelude

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
