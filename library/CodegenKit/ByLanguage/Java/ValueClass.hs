module CodegenKit.ByLanguage.Java.ValueClass where

import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude hiding (intercalate)

data Decl
  = Decl Builder

classDecl :: Text -> [Param] -> Decl
classDecl name params =
  Decl $
    [i|
      public final class $name {
        $propertyDecls

        public $name($constructorArgs) {
          $fieldAssignments
        }
      }
    |]
  where
    propertyDecls =
      intercalate "\n" . fmap propertyDecl $ params
      where
        propertyDecl (Param name type_ _) =
          [i|public final $type_ $name;|]
    fieldAssignments =
      intercalate "\n" . fmap fieldAssignment $ params
      where
        fieldAssignment (Param name _ _) =
          [i|this.$name = $name;|]
    constructorArgs =
      intercalate ", " . fmap arg $ params
      where
        arg (Param name type_ _) =
          [i|$type_ $name|]

data Param
  = Param
      !Text
      -- ^ Name.
      !Text
      -- ^ Type.
      !Text
      -- ^ Description.
