module CodegenKit.ByLanguage.Java.ValueClass
  ( contents,
    Decl,
    classDecl,
    Param,
    param,
  )
where

import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude hiding (intercalate)

contents :: Builder -> Decl -> Builder
contents namespace (Decl decl) =
  [i|
    package $namespace;

    $decl
  |]

data Decl
  = Decl Builder

classDecl :: Builder -> [Param] -> Decl
classDecl name params =
  Decl $
    [i|
      public final class $name {
        $propertyDecls

        public $name($constructorArgs) {
          $propertyAssignments
        }
      }
    |]
  where
    propertyDecls =
      intercalate "\n" . fmap paramPropertyDecl $ params
    propertyAssignments =
      intercalate "\n" . fmap paramAssignment $ params
    constructorArgs =
      intercalate ", " . fmap paramArg $ params

-- | Renderings of param in all contexts.
data Param = Param
  { paramPropertyDecl :: Builder,
    paramAssignment :: Builder,
    paramArg :: Builder
  }

param :: Text -> Text -> Param
param name type_ =
  Param
    [i|public final $type_ $name;|]
    [i|this.$name = $name;|]
    [i|$type_ $name|]
