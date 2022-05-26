module CodegenKit.ByLanguage.Java.ClassModule where

import Coalmine.MultilineTextBuilder
import qualified CodegenKit.ByLanguage.Java.ValueClass as ValueClass
import CodegenKit.Prelude hiding (intercalate)

newtype ClassModule
  = ClassModule (Text -> Text -> Text)

valueClass :: [ValueClass.Param] -> ClassModule
valueClass params =
  ClassModule $ \namespace className ->
    ValueClass.contents namespace $
      ValueClass.classDecl className params
