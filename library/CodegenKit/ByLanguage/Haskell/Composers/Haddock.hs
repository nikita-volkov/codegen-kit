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
  { lines :: Seq TextBuilder
  }

instance Semigroup Block where
  Block l <> Block r =
    Block $ case Seq.viewr l of
      Seq.EmptyR -> r
      lLines Seq.:> lEdgeLine -> case Seq.viewl r of
        Seq.EmptyL -> l
        rEdgeLine Seq.:< rLines ->
          (lLines Seq.|> (lEdgeLine <> rEdgeLine)) Seq.>< rLines

instance Monoid Block where
  mempty = Block mempty

instance IsString Block where
  fromString = textSplice . fromString

textSplice :: Text -> Block
textSplice = Block . Seq.fromList . fmap to . Text.lines

prependEachLineInSplice :: Line -> Block -> Block
prependEachLineInSplice (Line line) (Block lines) =
  Block $
    Seq.mapHead (line <>) lines

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
