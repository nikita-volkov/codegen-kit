module CodegenKit.Legacy.ByLanguage.Java.Attoparsec where

import Coalmine.AttoparsecExtras.Text
import CodegenKit.Legacy.ByLanguage.Java.Charsets qualified as Charsets
import CodegenKit.Legacy.Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.Text qualified as Text

namespace :: Parser [Text]
namespace =
  sepBy1 namespaceSegment separator
  where
    separator = char '.'

namespaceSegment :: Parser Text
namespaceSegment =
  Text.cons
    <$> charOfCharset Charsets.namespaceSegmentHead
    <*> textOfCharset Charsets.namespaceSegmentTail
