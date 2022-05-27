module CodegenKit.ByLanguage.Java.Attoparsec where

import Coalmine.AttoparsecExtras.Text
import qualified CodegenKit.ByLanguage.Java.Charsets as Charsets
import CodegenKit.Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import qualified Data.Text as Text

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
