module CodegenKit.JavaRenderer where

import Coalmine.EvenSimplerPaths (Path)
import Coalmine.MultilineTextBuilder
import CodegenKit.JavaRenderer.Model
import CodegenKit.Prelude hiding (intercalate)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified StructureKit.Charset as Charset

class ToCode a where
  toCode :: a -> Builder

instance ToCode Class where
  toCode Class {..} =
    case classTemplate of
      ProductClassTemplate elements ->
        [i|
          public final class $className {
            $propertyDecls

            public $className($constructorArgs) {
              $propertyAssignments
            }
          }
        |]
        where
          propertyDecls =
            intercalate "\n" . fmap map $ elements
            where
              map ProductElement {..} =
                [i|public final $productElementType $productElementName;|]
          propertyAssignments =
            intercalate "\n" . fmap map $ elements
            where
              map ProductElement {..} =
                [i|this.$productElementName = $productElementName;|]
          constructorArgs =
            intercalate ", " . fmap map $ elements
            where
              map ProductElement {..} =
                [i|$productElementType $productElementName|]
