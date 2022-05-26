module CodegenKit.ByLanguage.Java.ClassModule where

import Coalmine.EvenSimplerPaths (Path)
import Coalmine.MultilineTextBuilder
import qualified CodegenKit.ByLanguage.Java.ValueClass as ValueClass
import CodegenKit.Prelude hiding (intercalate)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified StructureKit.Charset as Charset

-- * --

classModuleFile :: ClassModule -> Namespace -> ClassName -> (Path, Text)
classModuleFile code namespace name =
  ( classFilePath namespace name,
    classModuleCode code namespace name
  )

classModuleCode :: ClassModule -> Namespace -> ClassName -> Text
classModuleCode (ClassModule code) namespace name =
  to $ code (namespaceCode namespace) (classNameCode name)

classFilePath :: Namespace -> ClassName -> Path
classFilePath namespace name =
  "src/main/java" <> namespacePath namespace <> classNameFilePath name

-- * --

data Namespace = Namespace
  { namespaceCode :: Builder,
    namespacePath :: Path
  }

textNamespace :: Text -> Either Text Namespace
textNamespace =
  fmap postProcess . parse parser
  where
    parser =
      Attoparsec.sepBy1 segment separator
      where
        segment = Text.cons <$> firstChar <*> remainder
          where
            firstChar = Attoparsec.satisfy (Charset.toCharPredicate charset)
              where
                charset =
                  Charset.charRange 'a' 'z'
            remainder = Attoparsec.takeWhile (Charset.toCharPredicate charset)
              where
                charset =
                  Charset.charRange 'a' 'z'
                    <> Charset.charRange 'A' 'Z'
                    <> Charset.charRange '0' '9'
                    <> "_"
        separator = Attoparsec.char '.'
    postProcess segments =
      Namespace code path
      where
        code = intercalate "." . fmap to $ segments
        path = foldMap read segments
          where
            read segment = case parseTextLeniently segment of
              Right path -> path
              Left err -> error $ "Oops! " <> to err

-- * --

data ClassName = ClassName
  { classNameCode :: Builder,
    classNameFilePath :: Path
  }

-- | Parse text as class name.
textClassName :: Text -> Maybe ClassName
textClassName text =
  case parseTextLeniently (text <> ".java") of
    Left err -> Nothing
    Right path -> Just $ ClassName (to text) path

-- * --

newtype ClassModule
  = ClassModule (Builder -> Builder -> Builder)

valueClass :: [ValueClass.Param] -> ClassModule
valueClass params =
  ClassModule $ \namespace className ->
    ValueClass.contents namespace $
      ValueClass.classDecl className params
